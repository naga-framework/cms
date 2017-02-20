-ifndef(AUTHOR_HRL).
-define(AUTHOR_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(author_keys,[user_id]).
-record(author, {?ITERATOR(feed, true),
           created        = 0      :: seconds(),
           user_id        = -1     :: fk_user(),
           status         = active :: active|suspended,
           name           = []     :: string(),
           bio            = []     :: string(),
           username       = []     :: string()        
          }).

-endif.