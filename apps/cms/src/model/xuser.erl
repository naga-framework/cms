-module(xuser).
-include_lib("kvs/include/metainfo.hrl").
-include("cms.hrl").
-compile(export_all).
-define(db,cms_db).
-define(model,?MODULE). %%record name

attribute_names()  -> ?db:attribute_names(?model).
attribute_names(_) -> ?db:attribute_names(?model).
attribute_types()  -> ?db:attribute_types(?model).
attribute_types(_) -> ?db:attribute_types(?model).
attribute_idx()    -> ?db:attribute_idx(?model).
attribute_idx(_)   -> ?db:attribute_idx(?model).

attributes(R)-> ?db:attributes(R).

public() -> attribute_names() -- 
  [container,feed_id,prev,next,feeds,password,avatar].

get(Email) when is_list(Email)-> 
  case kvs:index(?model,email,Email) of
    []  ->{error,notfound};
    [U] ->{ok,U};
    M   ->{error,notunique} 
  end;
get(Id) when is_integer(Id) -> 
  kvs:get(xuser,Id).

get(F,R)     -> ?db:get_value(F,R).
set(F,V,R)   -> ?db:set_value(F,V,R).
new()        -> ?db:new_record(?model).
new(L)       -> ?db:new(?model,L).
to_maps(R)   -> ?db:to_maps(R).
to_json(R)   -> ?db:to_json(R).

prepare_json(L) -> prepare_json(L,[]).
prepare_json([],Acc) -> Acc;
prepare_json([{K,undefined}|T],Acc) -> prepare_json(T,[{K,<<>>}]++Acc);
prepare_json([{K,V}|T],Acc) -> prepare_json(T,[{K,V}]++Acc).

% -----------------------------------------------------------------------------
% extract string
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
% render
% -----------------------------------------------------------------------------
render(avatar,undefined)    -> "<a class=\"user-profile\">"
                               "<img src=\"/static/assets/images/image.png\">"
                               "</a>";
render(avatar,V)            -> wf:f("<img src=\"~s\">",[V]);
render(_,undefined)         -> <<>>;
render(id,V)                -> V;
render(register_date,V)     -> V;
render(firstname,V)         -> V;
render(lastname,V)          -> V;
render(username,V)          -> V;
render(email,V)             -> V;
render(password,V)          -> "**********";
render(field,id)            -> "Id";
render(field,register_date) -> "Register date";
render(field,firstname)     -> "Firstname";
render(field,lastname)      -> "Lastname";
render(field,username)      -> "Username";
render(field,email)         -> "Email";
render(field,avatar)        -> "Avatar";
render(field,password)      -> "Password";
render(field,_)             -> "".

% -----------------------------------------------------------------------------
% before/after[save] before/after[update] before/after[delete]
% -----------------------------------------------------------------------------
before_create(R) ->
  Email = R:get(email),
  case xuser:get(Email) of
    {ok,U} -> {error, already_exist};
    {error,notfound} -> 
      Password = R:get(password),
      {ok,Cfg} = config:get(password),
      {Hmac, Salt, Iterations, DerivedLength} = Cfg:get(value),
      {ok,Key} = pbkdf2:pbkdf2(Hmac, Password, Salt, Iterations, DerivedLength),
      R1 = R:set(password,Key),
      {ok, R1}
  end.

before_update(Old,New) -> 
  case ?db:diff(Old,New) of
    [] -> {error, nodifference};
     Diff -> io:format("Diff ~p~n",[Diff]),
             {ok,New} 
  end.

save(R)    -> ?db:save(R).

update_password(Pass,R) ->
 {ok,Cfg} = config:get(password),
 {Hmac, Salt, Iterations, DerivedLength} = Cfg:get(value),
 {ok,Key} = pbkdf2:pbkdf2(Hmac, Pass, Salt, Iterations, DerivedLength),
 R1 = R:set(password,Key),
 R1:save().
% -----------------------------------------------------------------------------
% before/after[save] before/after[update] before/after[delete]
% -----------------------------------------------------------------------------
exist(Email) -> case kvs:index(?model,email,Email)of[]->false;_ ->true end.

check_credential(EnteredPassword,R) -> 
  Key = R:get(password),
  {ok,Cfg} = config:get(password),
  {Hmac, Salt, Iterations, DerivedLength} = Cfg:get(value),  
  case pbkdf2:pbkdf2(Hmac,EnteredPassword, Salt, Iterations, DerivedLength) of
    {ok, Key} -> true;
    _ -> false end.

% -----------------------------------------------------------------------------
% validations test before storing record.
% -----------------------------------------------------------------------------
validate(R) -> [Msg||{Test,Msg} <- validation_tests(R), not Test()].
validation_tests(R) -> 
  [{fun() -> notnull(R:get(email))  end,"Email required"},
   {fun() -> is_email(R:get(email)) end,"Email invalid"}].


notnull([]) -> false;
notnull(<<>>) -> false;
notnull(nill) -> false;
notnull(undefined) -> false;
notnull(_) -> true.

%from zotonic
is_email(Email) ->
    case re:run(Email, [$^|re()]++"$", [extended]) of
        nomatch   -> false;
        {match,_} -> true
    end.

re() ->
    "(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )".  