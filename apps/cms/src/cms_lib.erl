-module(cms_lib).

-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").
-include("cms.hrl").
-define(CREDIT,"CMS with Gentelella "
               "Bootstrap Admin Template, powered by "
               "<a href='http://github.com/naga-framework/naga'>naga-framework</a>").

notify(Type,Title,Msg) -> 
  gentelella:pnotify(Type,Title,Msg),
  ok.

redirect(Sec,Redirect) ->
  wf:wire(wf:f("setTimeout(function(){window.location='~s';}, ~B);",
          [Redirect,1000*Sec])). 

bindings(Identity, VendorsCSS, VendorsJS) ->               
  CSS = gentelella:vendors(css,VendorsCSS),
  JS  = gentelella:vendors(js,VendorsJS),
  #{is_admin := IsAdmin,
    is_author:= IsAuthor,
    is_moderator:= IsModerator,
    user:=User} = Identity,
  Access = [{id_admin, IsAdmin},{is_moderator,IsModerator},{is_author,IsAuthor}],
  io:format("Access ~p~n",[Access]),
   [
    {user,User:attributes()++[{access,Access}]},
    {app, [{name,?APP_NAME},
           {vsn,?APP_VSN},
           {credit, ?CREDIT}
           ]},
    {page,[{css,CSS},
           {title,"CMS Demo"},
           {js,JS}]}
   ].

bindings(VendorsCSS, VendorsJS) ->               
  CSS = gentelella:vendors(css,VendorsCSS),
  JS  = gentelella:vendors(js,VendorsJS),
   [
    {app, [{name,?APP_NAME},
           {vsn,?APP_VSN},
           {credit, ?CREDIT}
           ]},
    {page,[{css,CSS},
           {title,"CMS Demo"},
           {js,JS}]}
   ].