-module(cms_index).
-export([index/3, post/3, contact/3]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

%--------------------------------------------------------------------------------
% INDEX CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, [], _) -> 
  Articles = [begin 
              {ok,U} = xuser:get(A:get(author_id)),
              Name = U:get(username),
              [{id, A:get(id)},
               {author, Name},
               {heading, A:get(heading)},
               {subheading, A:get(subheading)},
               {publish_date, A:get(publish_date)}]
              end||A<-kvs:entries(kvs:get(feed,<<"post">>), article, 4)],

  Navigation = [N:to_maps()||N<-kvs:entries(kvs:get(feed,category), category, undefined)],

  Bindings = [{blog,[{name, config:get(blog_name)},
                     {desc, config:get(blog_desc)}]},
              {articles,Articles},
              {navigation, lists:reverse(Navigation)}],

  {ok, Bindings}.


post(<<"GET">>, [Article|_], _) -> 
  Bindings = cms_lib:bindings(), 
  {ok, Bindings}.


contact(<<"GET">>, _, _) ->
  Bindings = cms_lib:bindings(), 
  {ok, Bindings}.



%--------------------------------------------------------------------------------
% INTERNAL
%--------------------------------------------------------------------------------


