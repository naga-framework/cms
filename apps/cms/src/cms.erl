-module(cms).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

-include_lib("kvs/include/config.hrl").
-include("cms.hrl").
%-include("user.hrl").

main(A)    -> mad_repl:sh(A).
start(_,_) -> supervisor:start_link({local,cms }, cms,[]).
stop(_)    -> ok.
init([])   -> ensure_loaded(),
              kvs:join(),
              init_cms(),
              
              admin(wf:config(cms,adm_email,"admin@csm.naga"),
                    wf:config(cms,adm_username,"admin"),
                    wf:config(cms,adm_password,"123456")),

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
  admin(Email,Name,Pass).

admin(Email,Name,Pass) ->
  User  = m_user:new([{email,Email},
                      {username,Name},
                      {password,Pass}]),
  case User:save() of
    {ok, U} ->
      grant(U, admin),
      grant(U, author),
      grant(U, moderator);
    Err -> Err
  end.


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
   #feature{id={aprove,article},name="Aprove an article."},

   #feature{id={read,category} ,name="Read a category."},
   #feature{id={write,category},name="Create/Edit/Delete a category."},

   #feature{id={read,post}     ,name="Read a post."},
   #feature{id={write,post}    ,name="Create/Reply a post."}
  ].

revoke(User,Feature) -> cms_acl:define_access({user,User:get(email)}, {feature, Feature}, disable).
grant(User,Feature)  -> cms_acl:define_access({user,User:get(email)}, {feature, Feature}, allow).

admin(U)    -> grant(U,admin).
author(U)   -> grant(U,author).
moderator(U)-> grant(U,moderator).

revoke(User) -> [revoke(User,F)||F<-features()].

is_admin(U) when is_tuple(U)  -> is_admin(U:get(email)); 
is_admin(Id) when is_list(Id) -> 
  cms_acl:check_access({user,Id},{feature,admin}) =:= allow.
is_author(U) when is_tuple(U)  -> is_author(U:get(email));   
is_author(Id) when is_list(Id) -> 
  cms_acl:check_access({user,Id},{feature,author}) =:= allow.
is_moderator(U) when is_tuple(U)  -> is_moderator(U:get(email));
is_moderator(Id) when is_list(Id) -> 
  cms_acl:check_access({user,Id},{feature,moderator}) =:= allow.
is_blocked(U) when is_tuple(U)  -> is_blocked(U:get(email));
is_blocked(Id) when is_list(Id) -> 
  cms_acl:check_access({user,Id},{feature,blocked}) =:= allow.


