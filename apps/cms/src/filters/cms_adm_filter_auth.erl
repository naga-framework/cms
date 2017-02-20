-module(cms_adm_filter_auth).
-export([before_filter/2]).
-include_lib("n2o/include/wf.hrl").

before_filter(_,Ctx) -> 
  {Path,_} = cowboy_req:path(?REQ),
  case wf:user() of
    undefined -> wf:session(<<"after_auth_success">>, Path),
                 {redirect, {http, "/admin/login"}};
    Identity  -> {ok, Ctx#{ identity => Identity}} end.
