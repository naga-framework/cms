-module(adm_users).
-export([index/3, 
         profile/3,
         event/1
        ]).
-default_action(index).
-actions([index,profile]).

-include_lib("n2o/include/wf.hrl").
-include_lib("naga/include/naga.hrl").
-include_lib("cms/include/cms.hrl").

%--------------------------------------------------------------------------------
% INDEX
%--------------------------------------------------------------------------------
index(<<"GET">>, _, _)   -> 
  {501, <<"not implemented">>, []}.

%--------------------------------------------------------------------------------
% PROFILE
%--------------------------------------------------------------------------------
profile(<<"GET">>, _, #{identity:=Identity} = Ctx)   -> 

  io:format("Identity ~p~n",[Identity]),
  VendorsCSS = [bootstrap3,fontawesome,
                nprogress,icheck,prettify,select2,switchery,starrr,
                pnotify,gentelella],
  VendorsJS = [jquery,bootstrap3,fastclick,progressbar,
                nprogress,raphael,morris,icheck,moment,
                daterangepicker,wysiwyg,hotkeys,prettify,
                tagsinput,switchery,select2,parsley,autosize,
                autocomplete,pnotify,starrr,gentelella],
  Bindings = cms_lib:bindings(Identity,VendorsCSS,VendorsJS),

  {ok, Bindings}.
      


%--------------------------------------------------------------------------------
% EVENT HANDLING
%--------------------------------------------------------------------------------      
event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).