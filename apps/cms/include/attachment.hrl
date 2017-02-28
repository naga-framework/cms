-ifndef(ATTACHMENT_HRL).
-define(ATTACHMENT_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("cms_types.hrl").

-define(attachment_keys,[owner_id,md5]).
-record(attachment, {?ITERATOR(feed),
                      created        = 0    :: seconds(),
                      path           = []   :: list(), 
                      filename       = []   :: filename(),
                      thumbnail      = []   :: list(),    
                      owner_id       = -1   :: fk_user(),  
                      description    = []   :: text(),
                      meta           = []   :: list(),
                      mime           = []   :: mime(),
                      size           = 0    :: filesize(),
                      md5            = <<>> :: binary(),
                      is_deleted     = false:: boolean(),
                      extra          = []   :: list()
          }).

-endif.