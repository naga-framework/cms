-ifndef(USER_HRL).
-define(USER_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(xuser_keys,[email]).
-record(xuser, {?ITERATOR(feed),
           register_date  = 0   :: seconds(),
           avatar         = undefined :: fk(),
           firstname      = []  :: string(),
           lastname       = []  :: string(), 
           username       = []  :: string(),
           email          = []  :: email(), %{regex,<<"^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$">>}
           password       = <<>>:: binary() 
          }).


-endif.
