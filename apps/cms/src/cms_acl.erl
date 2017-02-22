-module(cms_acl).
-copyright('Synrc Research Center s.r.o.').
-compile(export_all).
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/acl.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("kvs/include/feed.hrl").
-include("user.hrl").

define_access(Accessor, Resource, Action) ->
    Entry = #access{ id={Accessor, Resource}, accessor=Accessor, action=Action},
    io:format("Entry ~p~n",[Entry]),
    case kvs:add(Entry) of
        {error, exist} -> kvs:put(Entry#access{action=Action});
        {error, no_container} -> skip;
        {ok, E} -> E end.

check(Keys) ->
    Acls = [Acl || {ok, Acl = #access{}} <- [kvs:get(access, Key) || Key <- Keys]],
    case Acls of
        [] -> none;
        [#access{action = Action} | _] -> Action end.

check_access(Accessor, Feature) ->
    Query = [ {Accessor,Feature} ],
    check(Query).

% check_access(Id, Feature) when is_list(Id)->
%     case kvs:get(m_user, Id) of
%         {ok, User} -> check_access(User, Feature);
%         E -> E end.
