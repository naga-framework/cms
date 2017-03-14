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
% INDEX CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, _, #{identity:=Identity} = Ctx)   -> 
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
  Bindings = cms_lib:bindings(Identity,VendorsCSS,VendorsJS),  
  {ok, Bindings}.


%--------------------------------------------------------------------------------
% EVENT HANDLING (ws)
%--------------------------------------------------------------------------------        
event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).