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
% CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, _, _)   -> 
  {501, <<"not implemented">>, []}.

profile(<<"GET">>, _, #{identity := Identity} = Ctx) -> 
  {ok,[{identity,maps:to_list(Identity)}]}.

event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).