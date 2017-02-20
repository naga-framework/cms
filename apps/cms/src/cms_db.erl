-compile({parse_transform, dynarec}).
-module(cms_db).
-compile(export_all).

-include_lib("kvs/include/metainfo.hrl").
-include("cms_types.hrl").
-include("cms.hrl").
-include("article.hrl").
-include("attachment.hrl").
-include("category.hrl").
-include("group.hrl").
-include("author.hrl").

-define(JSON(X), jiffy:encode(X)).
-define(attrs(X), record_info(fields,X)).
-define(table(X,Keys), #table{name=X ,container=feed,fields=?attrs(X),keys=Keys}).


limit(_Table,_Key)     -> 250000.

forbid(user)           -> 0;
forbid(____)           -> 100.

metainfo() ->
    #schema{name=cms,tables=[
         ?table(author     ,?author_keys)
        ,?table(category   ,?category_keys)
        ,?table(article    ,?article_keys)
        ,?table(attachment ,?attachment_keys)
        ,?table(group      ,?attachment_keys)
    ]}.


new(T,Attrs) when is_list(Attrs)->
    R = ?MODULE:new_record(T),
    lists:foldl(fun({F,V},N)->?MODULE:set_value(F,V,N)end,R,Attrs);

new(T,Attrs) when is_map(Attrs)->
    R = ?MODULE:new_record(T),
    lists:foldl(fun({F,V},N)->?MODULE:set_value(F,V,N)end,R,maps:to_list(Attrs)).


% attribute_names(T) -> ?MODULE:field_names(T).
% attribute_types(T) -> ?MODULE:field_types(T).

% attributes(R) when is_tuple(R) -> 
%     Fields = ?MODULE:field_names(element(1,R)),
%     lists:foldr(fun(F,Acc) -> 
%                     [{F, ?MODULE:get_value(F,R)}|Acc] 
%                 end, [], Fields).

% attribute_idx(T) ->
%     Fields = ?MODULE:field_names(T),
%     lists:zip(Fields,lists:seq(1,length(Fields))).

% to_maps(R) when is_tuple(R)-> maps:from_list(attributes(R)).
% to_json(R) -> T=element(1,R),
%               ?JSON({T:prepare_json(attributes(R))}).

