-module(cms_index).
-export([index/3]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

%--------------------------------------------------------------------------------
% INDEX CONTROLLER
%--------------------------------------------------------------------------------
index(<<"GET">>, [], _) -> 
  Bindings = [{blog,blog()},
              {articles,articles(<<"post">>,4)}, %%last 4 post
              {navigation, nav()}],
  {ok, Bindings};

index(<<"GET">>,[Cat|_]=Path,Ctx) -> 
  
  render(category:exist(Cat),Path,Ctx).

%--------------------------------------------------------------------------------
% INTERNAL
%--------------------------------------------------------------------------------
render(false,Path,Ctx)   -> {render_other, #{acction=>'404'}};
render({ok,C},[Name],Ctx)->
  io:format("~p:~p~n",[Name,article(Name)]),
  Bindings = [{blog,blog()},{category,C:to_maps()},{navigation, nav()},
              {article,article(Name)}],
  {render_other, #{action=>wf:to_atom(Name)},Bindings};
render({ok,C},[<<"post">>,Id],Ctx)->
  Bindings = [{blog,blog()},{category,C:to_maps()},{navigation, nav()},
              {article,article(<<"post">>,Id)}],
  {render_other, #{action=>post},Bindings}.  



articles(C,N) -> [begin 
                    {ok,U} = xuser:get(A:get(author_id)),
                    Name   = U:get(username),
                    [{id,           A:get(id)},
                     {author,       Name},
                     {heading,      A:get(heading)},
                     {subheading,   A:get(subheading)},
                     {publish_date, A:get(publish_date)}]
                  end||A<-kvs:entries(kvs:get(feed,C), article, N)].

article(N) -> case kvs:entries(kvs:get(feed,N), article, 1) of 
               [] -> [];[A] -> A:to_maps() end.

article(_,Id) -> case kvs:get(article,Id) of
                  {ok, R} -> R:to_maps();
                  _ -> [] end. 

blog()-> [{name, config:get(blog_name)},{desc, config:get(blog_desc)}].
nav() -> Cats = [N:to_maps()||N<-kvs:entries(kvs:get(feed,category), category, undefined)],
         io:format("~p~n",[Cats]),
         lists:reverse(Cats).

