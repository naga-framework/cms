-module(db_util).
-compile(export_all).

-include_lib("kvs/include/metainfo.hrl").
-define(THOUSAND, 1000).
-define(MILLION, ?THOUSAND*?THOUSAND).

hook_type(R) ->
  Id = R:get(id),
  Type = element(1,R),
  case Id == [] of
    true -> NId = kvs:next_id(Type,1),
            NR = R:set(id,NId),
            {create, Type, NR};
    false ->
            {update, Type, R}
  end.

save(Record) -> 
  save(Record, validate_record(Record)).

save(Record, ok) -> 
  {Hook, Type, R} = hook_type(Record),
  %io:format("isNew ~p : ~p~n",[Hook,R]),
  HookResult = ret(hook({before,Hook},Type,R),R),
  %io:format("HookResult ~p~n",[HookResult]),        
  case HookResult of
      {ok, R1} -> 
          case save_record(Hook, R1) of
            {ok, Saved} -> 
              ret(hook({'after',Hook},Type,Saved),Saved);
            Err -> Err
          end;
      Err -> Err
  end;
save(_, Errors) -> Errors. 

ret(ok,R) -> {ok,R};
ret({ok,R},_) -> {ok,R};
ret({error,_}=Err,_) -> Err.

hook({before,update},Type,R)->
  case erlang:function_exported(Type, before_update, 2) of
    true -> {ok,O} = Type:get(R:get(id)),
            ret(Type:before_update(O,R),R);
    false-> {ok,R}
  end;
hook({A,B},Type,R)->
  Fun = wf:atom([A,B]),
  case erlang:function_exported(Type, Fun, 1) of
    true -> ret(Type:Fun(R),R);
    false-> {ok,R}
  end.

delete(R) ->
Type = element(1,R),
 HookResult = ret(hook({before,delete},Type,R),R),
  case HookResult of
      {ok, R1} -> 
        case delete_record(Type,R1:get(id)) of
          ok -> ret(hook({'after',delete},Type,R1),R1);
          Err -> Err
        end;
      Err -> Err
  end.

%% add new record
save_record(create, R) -> kvs:add(R);
%% update a record
save_record(update, R) -> case kvs:put(R) of 
                          ok -> kvs:get(element(1,R),element(2,R));
                          Err -> Err end.

delete_record(Type,Id) -> kvs:delete(Type, Id).


hook_error({undef,[{_,set_value,[_,Field,_,_R],_}|_]}) -> {unknow_field,Field};
hook_error(Err) -> Err.


validate_record(Record) ->
    Type = element(1, Record),

    Errors1 = [],
    Errors2 = case Errors1 of
        [] ->
            case erlang:function_exported(Type, validation_tests, 1) of
                true -> [String || {TestFun, String} <- Record:validation_tests(), not TestFun()];
                false -> []
            end;
        _ -> Errors1
    end,
    case length(Errors2) of
        0 -> ok;
        _ -> {error, Errors2}
    end.

% validate_record(Record, IsNew) ->
%     Type = element(1, Record),
%     Action = case IsNew of
%                    true -> on_create;
%                    false -> on_update
%                end,
%     Errors = case erlang:function_exported(Type, validation_tests, 2) of
%                  % makes Action optional
%                  true -> [String || {TestFun, String} <- try Record:validation_tests(Action)
%                                                          catch error:function_clause -> []
%                                                          end,
%                                     not TestFun()];
%                  false -> []
%              end,
%     case length(Errors) of
%         0 -> ok;
%         _ -> {error, Errors}
%     end.


find(Type, Id) when is_integer(Id) -> kvs:get(Type,Id);
find(Type, {Index,K}) when is_atom(Type),is_atom(Index) -> 
  #table{keys=Indexes} = kvs:table(Type),
  case lists:member(Index,Indexes) of
    true -> kvs:index(Type,Index,K);
    false-> {error, unknow_index} 
  end;
find(Type, QH) -> find(Type, QH, []).

find(Type, QH, Conditions) when is_list(Conditions) ->
    find(Type, QH, Conditions, []).

find(Type, QH, Conditions, Options) ->
    Max = proplists:get_value(limit, Options, all),
    Skip = proplists:get_value(offset, Options, 0),
    Sort = proplists:get_value(order_by, Options, id),
    SortOrder = case proplists:get_value(descending, Options) of
        true -> descending;
        _ -> ascending
    end,
    %Include = proplists:get_value(include, Options, []),
    find(Type, QH, conditions(Conditions), Max, Skip, Sort, SortOrder).

find(Type, QH, Conditions, Max, Skip, Sort, SortOrder) 
  when is_atom(Type), is_list(Conditions),
       is_integer(Max) orelse Max =:= all,
       is_integer(Skip), 
       is_atom(Sort), 
       is_atom(SortOrder) ->

% Mnesia allows a pattern to be provided against which it will check records.
% This allows 'eq' conditions to be handled by Mnesia itself. The list of remaining
% conditions form a 'filter' against which each record returned by Mnesia is tested here.
% So...the first job here is to split the Conditions into a Pattern and a 'Filter'.

  {_, Filter}  = build_query(Type, Conditions, Max, Skip, Sort, SortOrder),
  RawList      = kvs:exec(QH),
  FilteredList = apply_filters(RawList, Filter),
  SortedList   = apply_sort(FilteredList, Sort, SortOrder),
  SkippedList  = apply_skip(SortedList, Skip),
  MaxList      = apply_max(SkippedList, Max),
  MaxList.

apply_filters(List, Filters) -> apply_filters(List, Filters, []).
apply_filters([],_Filters,Acc) -> Acc;
apply_filters([First|Rest],Filters,Acc) ->
    case filter_rec(First,Filters) of
        keep -> apply_filters(Rest,Filters,[First|Acc]);
        drop -> apply_filters(Rest,Filters,Acc)
    end.

filter_rec(_Rec, []) -> keep;
filter_rec(Rec, [First|Rest]) ->
    case test_rec(Rec, First) of
        true -> filter_rec(Rec,Rest);
        false -> drop
    end.

apply_sort([], _Key, _Order) -> [];
apply_sort(List, primary, Order) -> apply_sort(List, id, Order);
apply_sort(List, Key, ascending) ->
    Fun = fun (A, B) -> apply(A,Key,[]) =< apply(B,Key,[]) end,
    lists:sort(Fun, List);
apply_sort(List, Key, descending) ->
    Fun = fun (A, B) -> apply(A,Key,[]) >= apply(B,Key,[]) end,
    lists:sort(Fun, List).

apply_skip(List, 0) -> List;
apply_skip(List, Skip) when Skip >= length(List) -> [];
apply_skip(List, Skip) -> lists:nthtail(Skip, List).
apply_max(List, all) -> List;
apply_max(List, Max) when is_integer(Max) -> lists:sublist(List, Max).

test_rec(Rec,{Key, 'not_equals', Value}) -> apply(Rec,Key,[]) /= Value;
test_rec(Rec,{Key, 'in', Value}) when is_list(Value) -> lists:member(apply(Rec,Key,[]), Value) ;
test_rec(Rec,{Key, 'not_in', Value}) when is_list(Value) -> not lists:member(apply(Rec,Key,[]), Value) ;
test_rec(Rec,{Key, 'in', {Min, Max}}) when Max >= Min -> Fld = apply(Rec,Key,[]), (Fld >= Min) and (Fld =< Max);
test_rec(Rec,{Key, 'not_in', {Min, Max}}) when Max >= Min -> Fld = apply(Rec,Key,[]), (Fld < Min) or (Fld > Max);
test_rec(Rec,{Key, 'gt', Value}) -> apply(Rec,Key,[]) > Value;
test_rec(Rec,{Key, 'lt', Value}) -> apply(Rec,Key,[]) < Value;
test_rec(Rec,{Key, 'ge', Value}) -> apply(Rec,Key,[]) >= Value;
test_rec(Rec,{Key, 'le', Value}) -> apply(Rec,Key,[]) =< Value;
test_rec(Rec,{Key, 'matches', "*"++Value}) ->
    {ok, MP} = re:compile(Value, [caseless]),
    case re:run(apply(Rec,Key,[]), MP) of
        {match,_} -> true;
        _ -> false
    end;
test_rec(Rec,{Key, 'matches', Value}) ->
    {ok, MP} = re:compile(Value),
    case re:run(apply(Rec,Key,[]), MP) of
        {match,_} -> true;
        _ -> false
    end;
test_rec(Rec,{Key, 'not_matches', Value}) -> not test_rec(Rec,{Key, 'matches', Value});
test_rec(Rec,{Key, 'contains', Value}) -> lists:member(Value,apply(Rec,Key,[]));
test_rec(Rec,{Key, 'not_contains', Value}) -> not lists:member(Value,apply(Rec,Key,[]));
test_rec(Rec,{Key, 'contains_all', Values}) when is_list(Values) -> lists:all(fun (Ele) -> lists:member(Ele, apply(Rec,Key,[])) end, Values);
test_rec(Rec,{Key, 'not_contains_all', Values}) when is_list(Values) -> lists:any(fun (Ele) -> not lists:member(Ele, apply(Rec,Key,[])) end, Values);
test_rec(Rec,{Key, 'contains_any', Values}) when is_list(Values) -> lists:any(fun (Ele) -> lists:member(Ele, apply(Rec,Key,[])) end, Values);
test_rec(Rec,{Key, 'contains_none', Values}) when is_list(Values) -> lists:any(fun (Ele) -> not lists:member(Ele, apply(Rec,Key,[])) end, Values).



build_query(Type, Conditions, _Max, _Skip, _Sort, _SortOrder) -> % a Query is a {Pattern, Filter} combo
    %Fldnames = mnesia:table_info(Type, attributes),
    Fldnames = Type:fields(),
    BlankPattern = [ {Fld, '_'} || Fld <- Fldnames],
    {Pattern, Filter} = build_conditions(BlankPattern, [], Conditions),
    {[proplists:get_value(Fldname, Pattern) || Fldname <- Fldnames], Filter}.

build_conditions(Pattern, Filter, Conditions) ->
    build_conditions1(Conditions, Pattern, Filter).

build_conditions1([], Pattern, Filter) ->
    {Pattern, Filter};
build_conditions1([{Key, 'equals', Value}|Rest], Pattern, Filter) ->
    build_conditions1([{Key, 'eq', Value}|Rest], Pattern, Filter);
build_conditions1([{Key, 'eq', Value}|Rest], Pattern, Filter) ->
    build_conditions1(Rest, lists:keystore(Key, 1, Pattern, {Key, Value}), Filter);
build_conditions1([First|Rest], Pattern, Filter) ->
    build_conditions1(Rest, Pattern, [First|Filter]).


to_type(Val, undefined)                    -> Val;
to_type(Val, integer) when is_integer(Val) -> Val;
to_type(Val, integer) when is_list(Val)    -> list_to_integer(Val);
to_type(Val, integer) when is_binary(Val)  -> list_to_integer(binary_to_list(Val));
to_type(Val, float)   when is_binary(Val)  -> binary_to_float(Val);
to_type(Val, float)   when is_float(Val)   -> Val;
to_type(Val, float)   when is_integer(Val) -> 1.0 * Val;
to_type(Val, string)  when is_integer(Val) -> integer_to_list(Val);
to_type(Val, string)  when is_binary(Val)  -> binary_to_list(Val);
to_type(Val, string)  when is_list(Val)    -> Val;
to_type(Val, binary)  when is_integer(Val) -> list_to_binary(integer_to_list(Val));
to_type(Val, binary)  when is_list(Val)    -> list_to_binary(Val);
to_type(Val, binary) when is_binary(Val)   -> Val;

to_type({{D1, D2, D3}, {T1, T2, T3}}, Type) 
  when is_integer(D1), is_integer(D2), is_integer(D3),
       is_integer(T1), is_integer(T2), is_float(T3)
  -> to_type({{D1, D2, D3}, {T1, T2, round(T3)}}, Type);

to_type({{D1, D2, D3}, {T1, T2, T3}} = Val, integer) 
  when is_integer(D1), is_integer(D2), is_integer(D3),
       is_integer(T1), is_integer(T2), is_integer(T3)
  -> calendar:datetime_to_gregorian_seconds(Val);

to_type({{D1, D2, D3}, {T1, T2, T3}} = Val, timestamp) 
  when is_integer(D1), is_integer(D2), is_integer(D3),
       is_integer(T1), is_integer(T2), is_integer(T3)
       -> Secs = calendar:datetime_to_gregorian_seconds(Val) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
          {Secs rem ?MILLION, Secs div ?MILLION, 0};

to_type({{D1, D2, D3}, {T1, T2, T3}} = Val, datetime) 
  when is_integer(D1), is_integer(D2), is_integer(D3),
       is_integer(T1), is_integer(T2), is_integer(T3)    
  -> Val;

to_type({D1, D2, D3} = Val, date) 
  when is_integer(D1), is_integer(D2), is_integer(D3) -> Val;

to_type({date, {D1, D2, D3} = Val}, date) 
  when is_integer(D1), is_integer(D2), is_integer(D3) -> Val;

to_type(<<"1">>,     boolean) -> true;
to_type(<<"0">>,     boolean) -> false;
to_type(<<"true">>,  boolean) -> true;
to_type(<<"false">>, boolean) -> false;
to_type("1",         boolean) -> true;
to_type("0",         boolean) -> false;
to_type("true",      boolean) -> true;
to_type("false",     boolean) -> false;
to_type(1,           boolean) -> true;
to_type(0,           boolean) -> false;
to_type(true,        boolean) -> true;
to_type(false,       boolean) -> false.

conditions(Conditions) -> conditions(Conditions, []).
conditions([], Acc)    -> lists:reverse(Acc);
conditions([Key, Operator, Value|Rest], Acc) 
  when is_atom(Key), 
       is_atom(Operator) -> conditions(Rest, [{Key, Operator, Value}|Acc]);

conditions([{Key, Value}|Rest], Acc) 
  when is_atom(Key) -> conditions(Rest, [{Key, 'equals', Value}|Acc]);

conditions([{Key, 'eq', Value}|Rest], Acc) 
  when is_atom(Key) -> conditions(Rest, [{Key, 'equals', Value}|Acc]);

conditions([{Key, 'ne', Value}|Rest], Acc) 
  when is_atom(Key) -> conditions(Rest, [{Key, 'not_equals', Value}|Acc]);

conditions([{Key, Operator, Value}|Rest], Acc) 
  when is_atom(Key), is_atom(Operator) -> conditions(Rest, [{Key, Operator, Value}|Acc]);

conditions([{Key, Operator, Value, Options}|Rest], Acc) 
  when is_atom(Key), 
       is_atom(Operator), 
       is_list(Options) -> conditions(Rest, [{Key, Operator, Value, Options}|Acc]).


% table_exists(_, TableName) when is_atom(TableName) ->
%     lists:member(TableName, mnesia:table_info(schema, tables)).

% get_migrations_table(_) ->
%     mnesia:dirty_match_object({schema_migrations, '_', '_', '_'}).

% migration_done(_, Tag, up) ->
%     Id = "schema_migrations-" ++ integer_to_list(gen_uid(schema_migrations)),
%     RecordWithId = {schema_migrations, Id, atom_to_list(Tag), os:timestamp()},

%     Fun = fun() -> mnesia:write(schema_migrations, RecordWithId, write) end,

%     case mnesia:transaction(Fun) of
%         {atomic, ok} ->
%             ok;
%         {aborted, Reason} ->
%             {error, Reason}
%     end;
% migration_done(_, Tag, down) ->
%     case mnesia:dirty_match_object({schema_migrations, '_', atom_to_list(Tag), '_'}) of
%         [] ->
%             ok;
%         [Migration] ->
%             Id = element(2, Migration),
%             Fun = fun () -> mnesia:delete({schema_migrations,Id}) end,

%             case mnesia:transaction(Fun)  of
%                 {atomic,ok} ->
%                     ok;
%                 {aborted, Reason} ->
%                     {error, Reason}
%             end
%     end.
