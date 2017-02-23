-module(adm_dashboard).
-export([index/3, 
         event/1
        ]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").
-include("cms.hrl").

%--------------------------------------------------------------------------------
% CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, _, #{identity:=Identity} = Ctx)   -> 
  io:format("Identity ~p~n",[Identity]),
  {ok, bindings(Identity)}.


bindings(#{user:=User}) ->
  VendorsCSS = [bootstrap3,fontawesome,
                nprogress,icheck,progressbar,
                jqvmap,moment,daterangepicker,
                pnotify,gentelella],
  VendorsJS = [jquery,bootstrap3,fastclick,
                nprogress,chartjs,gauge,
                progressbar,icheck,skycons,flot,
                flot_orderbars,flot_spline,flot_curvedlines,
                datejs,jqvmap,moment,
                daterangepicker,pnotify,gentelella],                
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

            

event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).