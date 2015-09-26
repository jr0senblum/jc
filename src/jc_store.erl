%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2015, Jim Rosenblum
%%% @doc Library module that manages the mnesia tables mapping 
%%% {@link map_name(). Map} and {@link key(). Key} to a {@link value(). Value}. 
%%%
%%% IT IS UP TO THE CALLER TO WRAP THESE FUNCTIONS IN THE APPROPRIATE MNESIA 
%%% TRANSACTIONS
%%%
%%% This module interacts with mnesia to persist and access keys in service
%%% of the jc module.
%%%
%%% Up to four additional, indexed fields may be used to support faster 
%%% evict_match and values_match operations. These operations only make sense 
%%% for JSON values and look for Keys whose Values contain a specified value
%%% at a given 'path' within the JSON Value. That is, values_match and 
%%% evict_match select or delete based on a 'path' criteria, such as 
%%% "some.path.in.json.value=2". The additional fields, if used, are populated
%%% during a PUT operation -- the JSON Value is travesed for the target provided
%%% by the path criteria and the targetd value is put in an indxed field for 
%%% fast look up in service of the values_match and evict_match operations.
%%%
%%% {key_to_val, {Key, Map}, Map, Key, Value, i1, i2, i3, i4, ...}
%%% to_index contains a row for each {Map and JSON path to be indexed
%%% {to_index, {bed,{"some","path"}, bed, 6}
%%% the above record indicates that i1 (the 6th field) should be used
%%% to store the value at some.path in Values cached in the Bed map.
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(jc_store).


%% Jc_store public API
-export([clear/1,
	 evict/2, 
	 fun_match/4,
	 flush/1,
	 get/2,
	 get_map/1, get_map_since/2, key_set/1,
	 maps/0,
	 put/5]).

%% Meta-data API.
-export([up_nodes/0, stats/1]).

%% Custom-field indexing API.
-export([indexes/0, indexes/1, create_index/2, start_indexing/2, stop_indexing/2]).

%% Callback used by the jc_eviction manager to delete a cached value at TTL.
-export([delete_record_by_ref/1]).


 % Record and type definitions.
-include("../include/records.hrl").   

 % Use QLC for some querying.
-include_lib("stdlib/include/qlc.hrl").


-define(NO_SEQ, -1).
       

%% =============================================================================
%% Meta data API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% Return list of running mnesia nodes.
%%
-spec up_nodes() -> list(node()).

up_nodes()->
    mnesia:system_info(running_db_nodes).


%% -----------------------------------------------------------------------------
%% @doc Return vairous stats information. Currently size and uptime information.
%% 
-spec stats(size | up) -> {sizes, [{TableNm::atom(), 
				   {records, RecCnt::non_neg_integer()},
				   {bytes, Words::non_neg_integer()}}]} |
			  {uptime, term()} |
			  {error, not_found | badarg}.
stats(size) ->
    Data = 
	[{T, 
	  {records, mnesia:table_info(T, size)}, 
	  {bytes, mnesia:table_info(T, memory) * erlang:system_info(wordsize)}}
	 || T <-mnesia:system_info(tables)],
    {sizes, Data};

stats(up) ->
    case mnesia:dirty_read(stats, 'jc_store_up_time') of
	[#stats{value=Value}] ->
	    {uptime, Value};
	[] ->
	    {error, not_found}
    end;

stats(_) ->
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Return a sorted list of all maps currently in the cache.
%% 
-spec maps() -> [map_name()].

maps() ->
    QH = qlc:sort(
	   qlc:q([Map || #key_to_value{map=Map} <- 
			     mnesia:table(key_to_value)], {unique, true})
	  ),
    qlc:e(QH).



%% =============================================================================
%% Storage service API used by jc
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Clear all data for the given map.
%%
-spec clear(map_name()) -> ok.

clear(Map) ->
    F = fun() ->
		Items = mnesia:index_read(key_to_value, Map, #key_to_value.map),
		[mnesia:delete_object(Rec) || Rec <- Items],
		mnesia:delete({seq, Map})
	end,
     mnesia:sync_dirty(F),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Evict Map/Key from the cache
%% 
-spec evict(map_name(), key()) ->  ok.

evict(Map, Key) ->
    mnesia:delete({key_to_value, {Key, Map}}),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Delete the entire cache with or without triggering notifications based
%% on the parameter.
%%
-spec flush(silent | loud) ->  ok.

flush(silent) ->
    mnesia:clear_table(key_to_value),
    mnesia:clear_table(ttl),
    mnesia:clear_table(seq),
    mnesia:clear_table(auto_index),
    ok;

flush(loud) ->
    [mnesia:delete(key_to_value, Akey, write) || 
	Akey <- mnesia:all_keys(key_to_value)],

    mnesia:clear_table(ttl),
    mnesia:clear_table(seq),
    mnesia:clear_table(auto_index),
    ok.


%%------------------------------------------------------------------------------
%% @doc Return the #key_to_value{} which contains the data associated with 
%% {@link map_name(). Key} and {@link key(). Key}.
%% 
-spec get(map_name(), key()) -> {ok, key_to_value() | jc_miss}.

get(Map, Key) ->
    case mnesia:read(key_to_value, {Key, Map}) of
	[Rec] ->
	    {ok, Rec};
	[] ->
	    {ok, jc_miss}
    end.


%% -----------------------------------------------------------------------------
%% @doc Return all the #key_to_value() for a given {@link map_name(). Map}.
%% 
-spec get_map(Map::map_name()) -> {ok, [Results::key_to_value()]}.

get_map(Map) ->
    {ok, mnesia:index_read(key_to_value, Map, #key_to_value.map)}.


%% -----------------------------------------------------------------------------
%% @doc Return all keys in the given cache {@link map_name(). Map}.
%% 
-spec key_set(Map::map_name()) -> {ok, [Keys::key()]}.

key_set(Map) ->
    Q = qlc:q([R#key_to_value.key || R <- mnesia:table(key_to_value),
				     R#key_to_value.map == Map]),
    {ok, qlc:e(Q)}.


%% -----------------------------------------------------------------------------
%% @doc Return all the #key_to_value() for a given {@link map_name(). Map} 
%% whose create_tm is older than now - AgeSecs.
%%
-spec get_map_since(Map::map_name(), AgeSecs::seconds()) -> 
			   [Results::key_to_value()] | 
			   {error, module(), tuple() | bad_object}.

get_map_since(Map, AgeSecs) ->
    Limit = now_to_uepoch() - (AgeSecs * 1000000),
    Q = qlc:q([R || R <- mnesia:table(key_to_value),
		    R#key_to_value.map == Map,
		    R#key_to_value.create_tm < Limit]),
    qlc:e(Q).
		     

%% -----------------------------------------------------------------------------
%% @doc Put the {link @map_name(). Map} and {@link key(). Key} record.
%% Return a tuple with a reference to the record so that an eviction timer can
%% be created by the caller. IF THE PUT EVICTED AN ENTRY, JC_PSUB WILL DETECT
%% THIS, CANCEL THE OLD TIMER AND RAISE THE EVENT TO SUBSCRIBERS.
%% 
-spec put(map_name(), key(), value(), ttl(), rec_ref()) -> 
		 {ok, {put, rec_ref()}}.

put(Map, Key, Value, TTL, Ref) ->
    Time = now_to_uepoch(),
    Record = #key_to_value{jc_key      = {Key, Map},
			   map         = Map,
			   key         = Key, 
			   value       = Value,
			   create_tm   = Time,
			   last_update = Time,
			   ttl_secs    = TTL,
			   ref         = Ref},

    Augmented = add_indexes(Map, Value, Record),
    mnesia:write(Augmented),

    {ok, {put, Ref}}.



% -----------------------------------------------------------------------------
% Populate additional fields, configured for this map, with the value indicated
% by the configured JSON path in the Value JSON.
%
add_indexes(Map, Value, Record) ->
    case mnesia:index_read(to_index, Map, #to_index.map_name) of
	[] -> 
	    % nothing to do
	    Record;
	Indexes ->
	    try decode(Value) of
		JsonStruct ->
		    index(Indexes, JsonStruct, Record)
	    catch
		_:_ -> 
		    Record 
	    end
    end.


% ------------------------------------------------------------------------------
% For each JSON-path to index, get the value from the decoded JSON structure
% at the path, and add that value to the correct field in the record.
%
index([], _JStruct, Record) ->
    Record;
index([#to_index{map_path = {Map, Path}, position = Pos}|Rs], JStruct, Record)->
    try
	case ej_get(Path, JStruct) of
	    undefined   -> index(Rs, JStruct, Record);
	    jc_ej_error -> index(Rs, JStruct, Record);
	    JValue      -> 
		index(Rs, JStruct, setelement(Pos, Record, {Map, JValue}))
	end
    catch
	_:_ -> index(Rs, JStruct, Record)
    end.


%% -----------------------------------------------------------------------------
%% @doc Delete the cache element by its record reference. Used by eviction 
%% manager.
%%
-spec delete_record_by_ref(rec_ref()) ->  ok.

delete_record_by_ref(RecRef) ->
    case mnesia:index_read(key_to_value, RecRef, #key_to_value.ref) of
	[] ->
	    ok;
	Records  ->
	    F = fun(#key_to_value{jc_key=JCK}) ->
			mnesia:delete({key_to_value, JCK})
		end,
	    lists:foreach(F, Records)
    end.


%% -----------------------------------------------------------------------------
%% @doc Select those Map items whose JSON value, at the given path, equals Test 
%% and invoke the supplied function/4 with the Map, Key, Value and 
%% fold accumulator as parameters. 
%%
-spec fun_match(map_name(), Paths::tuple(), Test::atom(), Fun::fun()) -> 
		       [term()].


fun_match(Map, Path, Test, Fun) ->
    case try_index_match(Map, Path, Test, Fun) of
	undefined -> 
	    map_match(Map, Path, Test, Fun);
	Results -> 
	    Results
    end.


% ------------------------------------------------------------------------------
% If an entry in to_index indicates that the path is backed by an indexible
% custom field, use it to select only the records wich meet the criteria.
%
try_index_match(Map, Path, Test, Fun)->
    case index_get(Map, Path, Test) of
	undefined ->
	    lager:debug("~p: no index values found for ~p", 
			[?MODULE, {Map, Path}]),
	    undefined;
	Recs ->
	    lager:debug("~p: using index for ~p", [?MODULE, {Map, Path}]),
	    F = fun(#key_to_value{key = K, value = V}, Acc) ->
			Fun(Map, K, V, Acc)
		end,
	    lists:foldl(F, [], Recs)
    end.



% ------------------------------------------------------------------------------
% Do an index read based on the index-ed field indiated by the to_index record
% for the given Map and Json Path. If using the analyzer, update the statistics.
%
index_get(Map, Path, Test) -> 
    update_stats(Map, Path),
    case mnesia:read(to_index, {Map, Path}) of
	[] -> undefined;
	[#to_index{position = Pos}] ->
	    mnesia:index_read(key_to_value, {Map, Test}, Pos)
    end.


update_stats(Map, Path) ->
    jc_analyzer:update_statistic(Map, Path).


% ------------------------------------------------------------------------------
% No indexes to help so do it the expensive way: for all records in the map,
% decode each JSON value looking for the item indicated by Path and checking for 
% equality with Test.
%
map_match(Map, Path, Test, Fun) ->
    {ok, Recs} = get_map(Map),
    F = fun(#key_to_value{key=K, value=V}, Acc) ->
		case decode(V) of
		    jc_error -> 
			Acc;
		    Obj -> 

			Target = ej_get(Path, Obj),
			case Target of
			    Test ->
				Fun(Map, K, V, Acc);
			    _  -> 
				Acc
			end
		end;
	   (_, Acc) ->
		Acc		
	end,
    
    lists:foldl(F, [], Recs).


%% jsonx NIF doesnt compile on windows so use erlang jsone library

-ifdef('NO_NIF').
-define(JSON_DECODE(X), jsone:decode(X)).
-else.
-define(JSON_DECODE(X), jsonx:decode(X)).
-endif.

% decode the JSON using appropriate libary.
decode(Value) ->
    try
	V = case is_binary(Value) of
		true -> Value;
		false -> iolist_to_binary(Value)
	    end,
	?JSON_DECODE(V)
    catch 
	_:_-> jc_error
    end.



%% =============================================================================
%% Custom-field Indexing API
%% =============================================================================


% There are CUSTOM_CNT custom fields on the key_to_value record available for
% use as indexed fields to support JSON path-querries of the cach'ed Value.
-define(CUSTOM_FIELD_NAMES, [i1, i2, i3, i4]).
-define(CUSTOM_FIELD_POSITIONS, [#key_to_value.i1, 
				 #key_to_value.i2, 
				 #key_to_value.i3, 
				 #key_to_value.i4]).
-define(CUSTOM_CNT, length(?CUSTOM_FIELD_NAMES)).
-define(FIELD_NAME(I), lists:nth(I-#key_to_value.i1 + 1, ?CUSTOM_FIELD_NAMES)).



%% -----------------------------------------------------------------------------
%% @doc Start using a custom, field for the value found in JSON Values at
%% the path with respect to a given map.
%% Start indexing the bed map with respect to "menu.id" - 
%% start_indexing(bed, "menu.id"}).
%%
-spec start_indexing(map_name(), Path::string() | binary()) -> 
			    ok | {error, no_indexes_available | any()}.

start_indexing(Map, Path) ->
    case make_ej_path(Path) of
	error ->
	    lager:error("~p: could not start indexing ~p.", 
		       [?MODULE, {Map, Path}]);
	BPath ->
	    create_index(Map, BPath)
    end.



%% -----------------------------------------------------------------------------
%% @private used by {@link start_indexing/2. start_indexing/2} and also by
%% jc_analyzer to create indexes on json querries that have been used alot. This
%% function takes tuples as opposed to a string, dot-path specificationfor the
%% search criteria.
%%
-spec create_index(map(), tuple()) -> ok | {error, term()}.

create_index(Map, BPath) ->
    F = fun() ->
		mnesia:delete({to_index, {Map, BPath}}),
		case next_custom_field(Map) of
		    {error, _} = Error->
			Error;
		    {ok, NextPos} ->
			mnesia:write(#to_index{map_path = {Map, BPath},
					       map_name = Map,
					       position = NextPos}),
			clear(Map),
			NextPos
		end
	end,

    case mnesia:transaction(F) of
	{atomic, {error, E}} ->
	    lager:warning("~p: could not start indexing ~p: ~p.",
			  [?MODULE, {Map, BPath}, E]),
	    {error, E};

	{atomic, Pos} ->
	    lager:info("~p: to_index now includes ~p in pos. ~p.", 
		       [?MODULE, {Map, BPath}, Pos]),
            % didn't want the overhead of an indexed column unless
            % we are going to use it.
	    mnesia:add_table_index(key_to_value, ?FIELD_NAME(Pos)),
	    ok;

	{aborted, Reason} ->
	    lager:info("~p: could not start indexing ~p: ~p.", 
		       [?MODULE, {Map, BPath}, Reason]),
	    {error, Reason}
    end.


% ------------------------------------------------------------------------------
% Return the position of the next custom field available for use. Gather all 
% fields being use for a given map, subtrct the list of their positions from the
%  list of positions of all custom fields to see if there is an availabe field.
%
next_custom_field(Map) ->
    QH = qlc:q([R#to_index.position || R <- mnesia:table(to_index),
				       R#to_index.map_name == Map]),
    InUse = qlc:e(QH),
    case (?CUSTOM_FIELD_POSITIONS -- InUse) of
	[] -> 
	    {error, no_indexes_available};
	Available ->
	    {ok, hd(Available)}
    end.


%% -----------------------------------------------------------------------------
%% @doc Stop indexing Map entries with respect to the json path expressed as
%% tuples: {"menu", "id"}, for example. If no one is using a custom field, 
%% stop indexing it.
%%
-spec stop_indexing(map_name(), Path::string() | binary()) -> ok.

stop_indexing(Map, Path) ->
    case make_ej_path(Path) of
	error ->
	    lager:error("~p: could not stop indexing ~p.", 
		       [?MODULE, {Map, Path}]);
	BPath ->
	    lager:info("~p: updating to_index to exclude ~p.", 
		       [?MODULE, {Map, BPath}]),
	    mnesia:transaction(fun() -> mnesia:delete({to_index, {Map, BPath}}) end),
	    remove_unused_index(),
	    ok
    end.

% if no to_index row refers to a column, stop indexing that column.
remove_unused_index()->
    F = fun(Pos) ->
		case mnesia:dirty_index_read(to_index,Pos,#to_index.position) of
		    [] -> 
			mnesia:del_table_index(key_to_value, ?FIELD_NAME(Pos));
		    _ ->
			ok
		end
	end,
    _ = [F(Position) || Position <- ?CUSTOM_FIELD_POSITIONS].
		 

%% -----------------------------------------------------------------------------
%% Return all the JSON-path indexes being used.
%%
-spec indexes() -> {indexes, [{{map_name, tuple()}, Pos::non_neg_integer()}]}.

indexes() -> 
    QH = qlc:q([{R#to_index.map_path, R#to_index.position} 
		|| R <- mnesia:table(to_index)]),
    {indexes, mnesia:async_dirty(fun() -> qlc:e(QH) end )}.


%% -----------------------------------------------------------------------------
%% Return all the JSON-path indexes being used for a given map.
%%
-spec indexes(map_name()) -> {indexes, [{{map_name, tuple()}, Pos::non_neg_integer()}]}.

indexes(Map) ->
    QH = qlc:q([{R#to_index.map_path, R#to_index.position} 
		|| R <- mnesia:table(to_index),
	           R#to_index.map_name == Map]),
    {indexes, mnesia:async_dirty(fun() -> qlc:e(QH) end )}.



%% =============================================================================
%% Utility Functions
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Utility fn to return the number of micro-seconds since 1/1/1970 
%% -- UNIX-style epoch.
%%

now_to_uepoch() ->
    {A,B,C} = now(),
    ((A * 1000000 + B) * 1000000) + C.


% walk the path of a decoded JSON object using the ej library.
ej_get(Paths, Obj) ->
    try
	ej:get(Paths, Obj)
    catch
	_:_ -> jc_ej_error
    end.




% ------------------------------------------------------------------------------
% The Ej library navigates a decoded JSON structure via tuple descibing the 
% path to traverse. Elements are either an object key (string) or an index 
% into an array (number). 
%
-spec make_ej_path(string() | binary()) -> tuple() | error.

make_ej_path(DotString) when is_binary(DotString) -> 
    make_ej_path(binary_to_list(DotString));

make_ej_path(DotString) ->
    try 
	Tokenized = string:tokens(DotString, "."),
	list_to_tuple([to_path_elt(list_to_binary(Elt)) || Elt <- Tokenized])
    catch
	_:_ -> error
    end.

to_path_elt(<<"'", B/binary>>) -> 
    S = size(B) - 1,
    <<E:S/binary, _/binary>> = B,
    E;
to_path_elt(Element) ->    
    try binary_to_integer(Element)
    catch
	_:_ ->
	    Element
    end.

     



