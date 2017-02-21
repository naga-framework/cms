-compile({parse_transform, dynarec}).
-module(cms_db).
-compile(export_all).

-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/config.hrl").
-include_lib("kvs/include/group.hrl").
%-include_lib("kvs/include/comment.hrl").
%-include_lib("kvs/include/user.hrl").
%-include_lib("kvs/include/product.hrl").
%-include_lib("kvs/include/subscription.hrl").

-include("cms.hrl").
-include("article.hrl").
-include("attachment.hrl").
-include("category.hrl").
-include("user.hrl").

-define(JSON(X), jiffy:encode(X)).
-define(attrs(X), record_info(fields,X)).
-define(table(X,Keys), #table{name=X ,container=feed,fields=?attrs(X),keys=Keys}).


limit(_Table,_Key) -> 250000.
forbid(_)          -> 100.

metainfo() ->
    #schema{name=cms,tables=[
         ?table(m_user     ,?m_user_keys)
        ,?table(config     ,[])  
        ,?table(category   ,?category_keys)
        ,?table(article    ,?article_keys)
        ,?table(attachment ,?attachment_keys)
        ,?table(group      ,[])
    ]}.

new(T,Attrs) when is_list(Attrs)->
    R = ?MODULE:new_record(T),
    lists:foldl(fun({F,V},N)->?MODULE:set_value(F,V,N)end,R,Attrs);

new(T,Attrs) when is_map(Attrs)->
    R = ?MODULE:new_record(T),
    lists:foldl(fun({F,V},N)->?MODULE:set_value(F,V,N)end,R,maps:to_list(Attrs)).


attribute_names(T) -> ?MODULE:field_names(T).
attribute_types(T) -> ?MODULE:field_types(T).

attributes(R) when is_tuple(R) -> 
    Fields = ?MODULE:field_names(element(1,R)),
    lists:foldr(fun(F,Acc) -> 
                    [{F, ?MODULE:get_value(F,R)}|Acc] 
                end, [], Fields).

attribute_idx(T) ->
    Fields = ?MODULE:field_names(T),
    lists:zip(Fields,lists:seq(1,length(Fields))).

to_maps(R) when is_tuple(R)-> maps:from_list(attributes(R)).
to_json(R) -> T=element(1,R),
              ?JSON({T:prepare_json(attributes(R))}).

save(R) -> db_util:save(R).

diff(R1,R2) ->
  case {element(1,R1),element(1,R2)} of
    {T,T} ->
      [X||X<-lists:zipwith(fun({K,V},{K,V}) -> [];
                          ({K,V1},{K,V2}) -> {K,{V1,V2}} 
                    end, R1:attributes(), R2:attributes()),X/=[]];
    _ -> {error,badarg}
  end.