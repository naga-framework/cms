-module(cms_lib).

-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").
-include("cms.hrl").

bindings(#{user:=User}, VendorsCSS, VendorsJS) ->               
  CSS = gentelella:vendors(css,VendorsCSS),
  JS  = gentelella:vendors(js,VendorsJS),
   [
    {user,User:attributes()},
    {app, [{name,?APP_NAME},
           {vsn,?APP_VSN},
           {credit, "CMS with Gentelella "
                    "Bootstrap Admin Template, powered by "
                    "<a href='http://github.com/naga-framework/naga'>naga-framework"}
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
           {credit, "CMS with Gentelella "
                    "Bootstrap Admin Template, powered by "
                    "<a href='http://github.com/naga-framework/naga'>naga-framework"}
           ]},
    {page,[{css,CSS},
           {title,"CMS Demo"},
           {js,JS}]}
   ].