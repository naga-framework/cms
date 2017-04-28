-module(cms_index).
-export([index/3, post/3, before_filters/2]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

-define(CSS,[{blog, [bootstrap3,blog,fontawesome]}]).
-define(JS,[{blog, [jquery,bootstrap3,blog]}]).
  
%--------------------------------------------------------------------------------
% FILTER:
%--------------------------------------------------------------------------------
before_filters(Filters, _) -> Filters -- [cms_adm_filter_auth].

%--------------------------------------------------------------------------------
% INDEX CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, [], _) ->
 
  %%TODO: build first page, list at least 4 last post 
  Bindings = cms_lib:bindings(?CSS, ?JS),
  Articles = kvs:entries(kvs:get(feed,article), article, 4),


  {ok, Bindings ++ [{articles,Articles}]}.



post(<<"GET">>, [Article|_], _) ->
  io:format("Article ~p~n",[Article]), 
  Bindings = cms_lib:bindings(?CSS, ?JS), 
  {ok, Bindings}.



%--------------------------------------------------------------------------------
% INTERNAL
%--------------------------------------------------------------------------------


