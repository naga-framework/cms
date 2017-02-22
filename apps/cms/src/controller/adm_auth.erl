-module(adm_auth).
-export([login/3,logout/3,iforgot/3,register/3, 
         before_filters/2
         %event/1
        ]).
-default_action(index).
-actions([login,logout,iforgot,register]).

-include_lib("n2o/include/wf.hrl").
-include_lib("naga/include/naga.hrl").
-include_lib("cms/include/cms.hrl").

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
  {ok, loginBindins(BaseUrl)};

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
    {invalid, Raison} ->
      Bindings = loginBindins(BaseUrl)
                 ++ [{login_error, error_msg(Raison)}],
      {ok, Bindings} 
  end.

loginBindins(BaseUrl) -> 
   [{login_action, BaseUrl ++ ["/admin/login"]},
    {login_name, "CMS Admin Login"},
    {register_action, BaseUrl ++ ["/admin/register"]},
    {register_name, "CMS Admin Register"}].

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

%TODO: 
error_msg(_) -> "invalid credential".


%--------------------------------------------------------------------------------
% REGISTER
%--------------------------------------------------------------------------------
register(<<"GET">>, _, _)   ->
  {501, <<"not implemented">>, []};

register(<<"POST">>,_,_) ->
  #cx{form=PostData} = ?CTX,
  Username = proplists:get_value(<<"username">>,PostData),
  Email = proplists:get_value(<<"email">>,PostData),
  Pass = proplists:get_value(<<"password">>,PostData),
  ok.



%--------------------------------------------------------------------------------
% IFORGOT
%--------------------------------------------------------------------------------
iforgot(<<"GET">>, [<<"password">> =Type], #{'_base_url' := BaseUrl})   -> 
  io:format("IFORGOT ~p~n",[Type]),
  {501, <<"not implemented">>, []}.


%--------------------------------------------------------------------------------
% EVENT HANDLING
%--------------------------------------------------------------------------------
event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).

