-module(cms_index).
-export([index/3, stream/3, before_filters/2, event/1, test/1]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

%--------------------------------------------------------------------------------
% FILTER:
%--------------------------------------------------------------------------------
before_filters(Filters, _) -> Filters -- [cms_adm_filter_auth].

%--------------------------------------------------------------------------------
% INDEX
%--------------------------------------------------------------------------------
index(<<"GET">>, _, _)   -> 
  {ok, [{msg, "Hello World!!!!! "}]}.

stream(<<"GET">>, _, _)   -> 
  {stream, fun ?MODULE:test/1, 10}.

event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).


%--------------------------------------------------------------------------------
% TEST STREAM
%--------------------------------------------------------------------------------

test(A) when A>0 ->
  timer:sleep(500),
  {output, [integer_to_list(A)], A-1};

test(0) -> done.


