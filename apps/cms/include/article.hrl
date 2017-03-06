-ifndef(ARTICLE_HRL).
-define(ARTICLE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(article_keys,[heading_id]).  
-record(article, {?ITERATOR(feed), %% nitro have an article record also
           created        = 0     :: seconds(),
           modified       = 0     :: seconds(),
           author_id      = -1    :: fk_author(),
           publish_date   = 0     :: time(), %
           status         = draft :: draft | proposed | published | refused | dustbin,
           title          = []    :: string(),
           text           = []    :: string(),
           media          = []    :: list(),   %% list of media
           views          = 0     :: integer(),
           referrers      = []    :: list() 
          }).

-endif.