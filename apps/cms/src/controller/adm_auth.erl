-module(adm_auth).
-export([login/3,logout/3,iforgot/3, 
         before_filters/2,
         event/1
        ]).
-default_action(index).
-actions([login,logout,iforgot]).

-include_lib("n2o/include/wf.hrl").
-include_lib("naga/include/naga.hrl").
-include_lib("cms/include/cms.hrl").
-include_lib("nitro/include/nitro.hrl").

%--------------------------------------------------------------------------------
% FILTER:
%--------------------------------------------------------------------------------
before_filters(Filters, _) -> Filters -- [cms_adm_filter_auth].


%--------------------------------------------------------------------------------
% LOGOUT
%--------------------------------------------------------------------------------
logout(_,_,_)  -> wf:logout(),
                  {redirect, "/admin/login"}.

%--------------------------------------------------------------------------------
% LOGIN
%--------------------------------------------------------------------------------
login(<<"GET">>, _, #{'_base_url' := BaseUrl}) -> 
  {ok, bindings(BaseUrl)};

login(<<"POST">>, _, #{'_base_url' := BaseUrl}) ->
  #cx{form=PostData} = ?CTX,
  Username = proplists:get_value(<<"email">>,PostData),
  Password = proplists:get_value(<<"password">>,PostData),
  case check_user(Username,Password) of
    {ok, User} -> 
        Identity = identity:new(User),
        wf:user(Identity),
        case wf:session({on,auth,success}) of
          undefined    -> {redirect, "/admin"};
          {redirect,R} -> {redirect, R} 
        end;
    {invalid, _}=Err ->
      {ok, bindings(BaseUrl)++[{errMsg,error_msg(Err)}]} 
  end.

bindings(BaseUrl) ->
  Vendors = [jquery,bootstrap3,fontawesome,nprogress,animate,pnotify],
  CSS = gentelella:vendors(css,Vendors),
  JS  = gentelella:vendors(js,Vendors),
   [{login_action, BaseUrl ++ ["/admin/login"]},
    {login_name, "CMS Admin Login"},
    {register_action, BaseUrl ++ ["/admin/register"]},
    {register_name, "CMS Admin Register"},
    {wsFormRegister, wsFormRegister()},
    {page,[{css,CSS},{js,JS}]}
   ].

check_user(Email,Pass) when is_binary(Email)->
  check_user(wf:to_list(Email),Pass);
check_user(Email,Pass) ->
 case m_user:get(Email) of
  {ok,User} -> 
    case User:check_credential(Pass) of
      true  -> {ok, User};
      false -> {invalid, credential}
    end;
  _ -> {invalid,user} 
 end.




%--------------------------------------------------------------------------------
% IFORGOT
%--------------------------------------------------------------------------------
iforgot(<<"GET">>, [<<"password">> =Type], #{'_base_url' := BaseUrl})   -> 
  io:format("IFORGOT ~p~n",[Type]),
  {501, <<"not implemented">>, []}.


%--------------------------------------------------------------------------------
% NOTIFY
%--------------------------------------------------------------------------------
notify(Type,Title,Msg) -> 
  gentelella:pnotify(Type,Title,Msg),
  ok.
 
error_msg({invalid,user}) -> "invalid user.";
error_msg({invalid,credential}) -> "invalid credential.";
error_msg({error,already_exist}) -> "user already exist.".

%--------------------------------------------------------------------------------
% EVENT HANDLING
%--------------------------------------------------------------------------------
% REGISTER
%--------------------------------------------------------------------------------
% in gentelella register/login are in the same view
% good occasion here to implement register with websocket
event(register) -> 
  Params  = lists:foldl(fun(X,Acc)->
                          [{X,wf:to_list(wf:q(X))}|Acc]
                        end,[],[email,username,password]),
  User    = m_user:new(Params),
  case User:save() of
   {ok, U} -> {redirect, "/admin/login"};
   Err     -> notify(error,"Register",error_msg(Err))
  end;
event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).


%--------------------------------------------------------------------------------
% 
%--------------------------------------------------------------------------------
wsFormRegister() -> 
Actions = "",
wf:render(
 #form{actions=Actions,body=[
  #h1{body=["CMS Register"]},
  #panel{body=[
      #input{class=["form-control"],
             id="username", 
             type=text, 
             placeholder="Username", data_fields=[{required,<<>>}]}
    ]},
  #panel{body=[
      #input{class=["form-control"],
             id="email", 
             type=text, 
             placeholder="Email", data_fields=[{required,<<>>}]}
    ]},
  #panel{body=[
      #input{class=["form-control"],
             id="password", 
             type=password,  
             placeholder="Password", data_fields=[{required,<<>>}]}
    ]},
  #panel{body=[
      #button{class=["btn btn-default submit"], 
             postback=register, source=[username,email,password],
             body=["submit"]
             }
    ]},
  #panel{class=[clearfix]},
  #panel{class=[separator],body=[
    #p{class=[change_link],body=[
      "Already a member ?",
      #link{ href=["#signin"], 
             class=[to_register], body=["Log in"]}
    ]},
    #panel{class=[clearfix]},
    #br{},
    #panel{body=[
      #h1{body=[
        #i{class=["fa fa-paw"]}, "Gentelella Alela!"
      ]},
      #p{body=["©2016 All Rights Reserved. Gentelella Alela! is a Bootstrap 3 template. Privacy and Terms"]}
    ]}
  ]}    
 ]}).