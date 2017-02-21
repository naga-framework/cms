-module(cms).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

-include_lib("kvs/include/config.hrl").
-include("cms.hrl").

main(A)    -> mad_repl:sh(A).
start(_,_) -> supervisor:start_link({local,cms }, cms,[]).
stop(_)    -> ok.
init([])   -> ensure_loaded(),
              kvs:join(),
              init_cms(),
              naga:start([cms]), 
              naga:watch([cms,gentelella]),
              sup().
sup()      -> { ok, { { one_for_one, 5, 100 }, [] } }.

log_modules() -> [n2o_client,
                  n2o_nitrogen,
                  n2o_stream,
                  wf_convert,
                  cms_index,
                  cms_error,
                  cms,
                  cms_db].

ensure_loaded() ->
  case application:get_key(cms,modules) of 
    undefined -> skip;
    {ok,Modules} -> [code:ensure_loaded(M)||M<-Modules] 
  end.

init_cms() ->
  case config:get(status) of 
    {error,not_found} -> 
      lists:map(fun({K,V}) -> 
                  Cfg = config:new([{key,K},{value,V}]),
                  Cfg:save()
                end,config());
    _ -> skip 
  end.

admin() ->        
  Email = getinput("   email: "),
  Name  = getinput("username: "),
  Pass  =  getpass("password: "),
  User  = m_user:new([{email,Email},
                      {username,Name},
                      {password,Pass}]),
  {ok, U} = User:save(),
  grant({user, U:get(email)}, admin).


getinput(Prompt) -> 
    Input = io:get_line(Prompt),
    case lists:reverse(Input) of
        [$\n | Rest] ->
            lists:reverse(Rest);
        _ ->
            Input
    end.

getpass(Prompt) -> 
    InitialIOOpts = io:getopts(),
    ok = io:setopts([{echo, false}]),
    Input = io:get_line(Prompt),
    ok = io:setopts(InitialIOOpts),
    io:format("\n"),
    case lists:reverse(Input) of
        [$\n | Rest] ->
            lists:reverse(Rest);
        _ ->
            Input
    end.

config() ->
   [{features, features()},
    {status,   init},
    {node,     node()},
    {nodes,    nodes()},
    {app_name, ?APP_NAME},
    {app_vsn,  ?APP_VSN},
    {password, {sha256, <<"salt">>, 4096, 20}}
   ].

features() -> 
  [#feature{id={read,article}  ,name="Read an article."},
   #feature{id={write,article} ,name="Create/Edit/Delete article."},

   #feature{id={read,category} ,name="Read a category."},
   #feature{id={write,category},name="Create/Edit/Delete a category."},

   #feature{id={read,post}     ,name="Read a post."},
   #feature{id={write,post}    ,name="Create/Reply a post."}
  ].

revoke(User,Feature) -> kvs_acl:define_access(User, {feature, Feature}, disable).
grant(User,Feature)  -> kvs_acl:define_access(User, {feature, Feature}, allow).

admin(U) -> [grant({user,U},F)||{F,_}<- features()].
revoke(User) -> [revoke(User,F)||F<-features()].

