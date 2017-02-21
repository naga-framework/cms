-ifndef(ARTICLE_HRL).
-define(ARTICLE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(article_keys,[heading_id]).  
-record(article, {?ITERATOR(feed, true), %% nitro have an article record also
           created        = 0     :: seconds(),
           modified       = 0     :: seconds(),
           publish_date   = 0     :: time(), %
           status         = draft :: draft | proposed | published | refused | dustbin,
           title          = []    :: string(),
           top_title      = []    :: string(),
           sub_title      = []    :: string(),
           description    = []    :: string(),
           text           = []    :: string(),
           ps             = []    :: string(), %% post-scriptum
           media          = []    :: list(),   %% list of media
           views          = 0     :: integer(),
           referrers      = []    :: list() 
          }).

-endif.