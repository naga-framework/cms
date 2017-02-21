-ifndef(USER_HRL).
-define(USER_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(m_user_keys,[email]).
-record(m_user, {?ITERATOR(feed, true),
           register_date  = 0   :: seconds(),
           username       = []  :: string(),
           email          = []  :: email(), %{regex,<<"^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$">>}
           password       = <<>>:: binary() 
          }).


-endif.
