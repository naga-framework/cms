-ifndef(CMS_TYPES_HRL).
-define(CMS_TYPES_HRL, true).

%%TODO: typed record => generate validator using proper ?
-type foreign_key() :: integer().
-type fk()          :: foreign_key().
-type fk_category() :: foreign_key().
-type fk_user()     :: foreign_key().
-type fk_page()     :: foreign_key().
-type fk_product()  :: foreign_key().

-type mime()        :: binary(). %%
-type filesize()    :: integer().
-type seconds()     :: integer().
-type text()        :: string().
-type time()        :: {0..23,0..59,0..59}.
-type path()        :: string().
-type filename()    :: string().
-type markdown()    :: string().

-endif.