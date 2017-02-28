-ifndef(CATEGORY_HRL).
-define(CATEGORY_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(category_keys,[path]).
-record(category, {?ITERATOR(feed),
           created        = 0  :: seconds(),
           modified       = 0  :: seconds(),
           path           = [] :: path(),
           title          = [] :: string(),
           description    = [] :: string(),
           text           = [] :: text()
          }).

-endif.