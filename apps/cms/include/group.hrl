-ifndef(GROUP_HRL).
-define(GROUP_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(group_keys,[name]).
-record(group, {?ITERATOR(feed, true),
                created  = 0  :: seconds(),
                name     = [] :: string()
          }).

-endif.