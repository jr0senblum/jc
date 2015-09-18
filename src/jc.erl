%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2015, Jim Rosenblum
%%% @doc This module wraps the mnesia-interacting, lower-level functions
%%% implemented in {@link jc_store. jc_store} to provide a public, DIRTY,
%%% set of opperations. {@link jc_s. jc_s} provides functions that take a 
%%% sequence parameter to better support serilization (consistency).
%%%
%%% jc can be called directly by Erlang clients; or,
%%% Java node -> JInterface -> {@link jc_bridge. jc_bridge} -> jc
%%% 
%%% @version {@version}
%%% @end
%%% Created : 16 December 2011 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(jc).


% Put Functions
-export([put/3, put/4,
	 put_all/2, put_all/3]).

% Delete Functions
-export([clear/1,
	 evict/2, evict_match/2, evict_all_match/1, evict_map_since/2, 
	 flush/0, flush/1,
         remove_items/2, 
	 delete_record_by_ref/1]).

% Get Functions
-export([contains_key/2,
	 get/2,
	 get_all/2,
	 key_set/1,
	 values/1,
	 values_match/2]).

% CACHE META-INFO SUPPORT
-export([cache_nodes/0, cache_size/0, map_size/1, maps/0, up/0, stop/0]).


% Used by jc_s for evict_match
-export([fun_match/3]).

% definitions of persisted and global records and types.
-include("../include/records.hrl").


-define(INFINITY, 0).
-define(VALID(X), is_integer(X) andalso (X >= 0)).


%% =============================================================================
%% Meta data API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Stop the cache at this node.
%% 
-spec stop() -> ok.

stop()->
    application:stop(jc),
    application:stop(ranch),
    ok.
    

%% -----------------------------------------------------------------------------
%% @doc Return a sorted list of all maps currently in the cache. 
%% 
-spec maps() -> [map_name()].

maps() -> 
    trans_execute(fun() -> jc_store:maps() end).
			  

%% -----------------------------------------------------------------------------
%% @doc Returns the number of objects in the map.
%% 
-spec map_size(map_name()) -> {records, non_neg_integer()}.

map_size(Map)->
    {ok, Results} = trans_execute(fun()->jc_store:key_set(Map) end),
    {records, length(Results)}.


%% -----------------------------------------------------------------------------
%% @doc Returns table size information, in records and words, for all tables
%% used by j_cache. Notice that 'cached' {@link key(). Key} 
%% {@link value(). Value} data are stored in the key_to_value table.
%%
-spec cache_size() -> {size, [{TableNm::atom(), {records, non_neg_integer()},
			                        {bytes, non_neg_integer()}}]}.

cache_size()->
    jc_store:stats(size).


%% -----------------------------------------------------------------------------
%% @doc Returns the date of cluster creation and uptime.
%% 
-spec up() -> {'uptime',[{'now',_} | {'up_at',_} | {'up_time',{_,_}},...]}.

up() ->
    {uptime, Start} = jc_store:stats(up),
    StartSecs =  calendar:datetime_to_gregorian_seconds(Start),

    Now =  calendar:now_to_datetime(now()),
    NowSecs =  calendar:datetime_to_gregorian_seconds(Now),

    Uptime = calendar:seconds_to_daystime(NowSecs-StartSecs),

    {uptime, [{up_at, httpd_util:rfc1123_date(Start)},
	      {now, httpd_util:rfc1123_date(Now)},
	      {up_time, Uptime}]}.


%% -----------------------------------------------------------------------------
%% @doc Return all 'up' JCache nodes and all configured JCache node. A node is
%% considered to be up if Mnesia and jc_bridge are both up.
%%
-spec cache_nodes() -> {nodes, {active, [node()]}, {configured, [node()]}}.

cache_nodes() ->
    Configured = application:get_env(jc, cache_nodes,[]),
    MnesiaUp = jc_store:up_nodes(),
    Running = [N || N <- MnesiaUp, 
		    undefined /= rpc:call(N,erlang,whereis,[jc_bridge], 1000)],
    {nodes, {active,lists:sort(Running)},{configured, lists:sort(Configured)}}.



%% =============================================================================
%% PUT API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Put the entry into the cache with a TTL of infinity.
%% 
-spec put(map_name(), key(), value()) -> {ok, {key, key()}} | {error, badarg}.

put(Map, Key, Value) ->
    put(Map, Key, Value, ?INFINITY).


%% -----------------------------------------------------------------------------
%% @doc Put the entry into the cache with the supplie TTL.
%%
-spec put(map_name(), key(), value(), ttl()) -> {ok, {key, key()}} |
						{error, badarg}.

put(Map, Key, Value, TTL) when ?VALID(TTL) ->
    lager:debug("~p: put (~p, ~p) with TTL: ~p.", [?MODULE, Map, Key, TTL]),

    Ref = make_ref(),
    F = fun() -> jc_store:put(Map, Key, Value, TTL, Ref) end,
    {ok, {put, Ref}} = trans_execute(F),
    {ok, {key, Key}};

put(_M, _K, _V, _T) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Put all the {K,V} tuples contained in the list. Return 
%% the number of successes. Use infinity for the ttl. 
%%
-spec put_all(map_name(), list(tuple())) -> {ok, {cnt, non_neg_integer()}} |
					    {error, badarg}.

put_all(Map, KVList) -> 
    put_all(Map, KVList, ?INFINITY).


%% -----------------------------------------------------------------------------
%% @doc Put all the {K,V} pairs contained in the list. Return the number of 
%% successes. Use the supplied ttl. 
%%
-spec put_all(map_name(), list(tuple()), ttl()) -> {ok,{cnt,non_neg_integer()}}|
						   {error, badarg}.

put_all(Map, KVList, TTL) when ?VALID(TTL) ->
    lager:debug("~p: put_all for map ~p with TTL: ~p.", [?MODULE, Map, TTL]),

    Results = [put(Map, Key, Value, TTL) || {Key, Value} <- KVList],
    {ok, {cnt, length([K || {ok, {key, K}} <- Results])}};
put_all(_m, _K, _T) ->
    {error, badarg}.





%% =============================================================================
%%  DELETE API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc Evict all data associated with the supplied {@link map_name(). Map}.
%% 
-spec clear(map_name()) -> ok.

clear(Map) ->
    lager:debug("~p: clear map ~p.", [?MODULE, Map]),
    jc_store:clear(Map),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Evict {@link map_name(). Map}, {@link key(). Key}.
%% 
-spec evict(map_name(), key()) -> ok.

evict(Map, Key) ->
    lager:debug("~p: evict (~p, ~p).", [?MODULE, Map, Key]),
    F = fun() -> jc_store:evict(Map, Key) end,
    trans_execute(F),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Evict Map/Key from the cache for Key's whose value matches the criteria.
%% Assumes the the criteria is a string in the form of "a.b.c=true", where 
%% a.b.c is dot-path consisting of dot-separated JSON object-keys or JSON array
%% indexes: "bed.id=10" or "bed.id.2.type.something=\"stringvalue\"".
%%
-spec evict_match(map_name(), Criteria::string()) ->  ok.

evict_match(Map, Criteria) ->
    lager:debug("~p: evict_match with map: ~p, and criteria: ~p.", 
		[?MODULE, Map, Criteria]),
    Fun = fun(M, K, _, Acc) ->  evict(M, K), Acc end,
    fun_match(Map, Criteria, Fun),
    ok.



%% -----------------------------------------------------------------------------
%% @doc Call {@link evict_match/2} for each Map.
%% Assumes the the criteria is a string in the form of "a.b.c=true", where 
%% a.b.c is dot-path consisting of dot-separated JSON object-keys or JSON array
%% indexes: "bed.id=10" or "bed.id.2.type.something=\"stringvalue\"".
%%
-spec evict_all_match(Criteria::string()) ->  ok.

evict_all_match(Criteria) ->
    lager:debug("~p: evict_all_match ~p.", [?MODULE, Criteria]),
    _ = [evict_match(M, Criteria) || M <- maps()],
    ok.



%% -----------------------------------------------------------------------------
%% @doc Evict all items in the map whose create_tm is older than Age seconds.
%% 
-spec evict_map_since(map_name(), AgeSecs::seconds()) -> ok.

evict_map_since(Map, AgeSecs) when is_integer(AgeSecs) ->
    lager:debug("~p: evict_map_since: ~p older than ~p.",
		[?MODULE, Map, AgeSecs]),
    Trans = fun() ->
		    Recs = jc_store:get_map_since(Map, AgeSecs),
		    [jc:evict(Map, R#key_to_value.key) ||
			R <- Recs]
	    end,
    trans_execute(Trans),
    ok;

evict_map_since(_Map, _AgeSecs) ->
    ok.


%% -----------------------------------------------------------------------------
%% @doc Delete the cache element by its record reference. Used by 
%% {@link jc_eviction_manager. jc_eviction_manager}.
%% 
-spec delete_record_by_ref(rec_ref()) -> ok | {error, mnesia_abort}.

delete_record_by_ref(RecRef) ->
    lager:debug("~p: delete_record_by_ref: ~p.",[?MODULE, RecRef]),
    F = fun() -> jc_store:delete_record_by_ref(RecRef) end,
    trans_execute(F).


%% -----------------------------------------------------------------------------
%% @doc Remove all existing items from the cache sending subscription
%% messages to subscribers.
%% 
-spec flush() -> ok.

flush() ->
    do_flush(loud).


%% -----------------------------------------------------------------------------
%% @doc Remove all existing items from the cache without triggering 
%% notifications.
%% 
-spec flush(silent) -> ok.

flush(silent) ->
    do_flush(silent);
flush(_) ->
    {error, badarg}.



do_flush(Type)->
    F = case Type of
	silent ->
	    lager:debug("~p: courtesy flush.", [?MODULE]),
		fun() -> jc_store:flush(silent) end;
	    _ ->
		lager:debug("~p: flush.", [?MODULE]),
		fun() -> jc_store:flush(loud) end
	end,
    trans_execute(F),
    ok.



%% -----------------------------------------------------------------------------
%% @doc Evict all K's, return all {K, V} pairs that were found.
%%
-spec remove_items(Map::map_name(), Keys::[key()]) -> {ok, [{key(), value()}]}.

remove_items(Map, Keys)->
    lager:debug("~p: remove_items (~p, ~p).",[?MODULE, Map, Keys]),

    F = fun(Key, Acc) ->
		case jc_store:get(Map, Key) of
		    {ok, #key_to_value{value = Value}} ->
			evict(Map, Key),
			[{Key, Value} | Acc];
		    {ok, jc_miss} ->
			Acc
		end
	end,
    
    Result = trans_execute(fun()->lists:foldl(F, [], Keys)end),
    {ok, Result}.



%% =============================================================================
%% Get API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Return true if the {@link key(). Key} is in the map, else false.
%%
-spec contains_key(Map::map_name(), Key::key()) -> true | false.

contains_key(Map, Key) ->
    case get(Map, Key) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.


%% -----------------------------------------------------------------------------
%% @doc Retrieve the data associated with Key.
%%						%
-spec get(map_name(), key()) -> {ok, {value, value()}} | miss.

get(Map, Key) ->
    lager:debug("~p: get (~p, ~p).",[?MODULE, Map, Key]),

    F = fun() -> jc_store:get(Map, Key) end,
    case trans_execute(F) of
	{ok, jc_miss} -> 
	    miss;
	{ok, #key_to_value{value=Value}} -> 
	    {ok, {value, Value}}
    end.


%% -----------------------------------------------------------------------------
%% @doc Return a list of {K, V} for each supplied key that was found in the
%% chache, and a list of Ks that were misses.
%%
-spec get_all(map_name, [key()]) -> {ok, {[{key(), value()}], [key()]}}.

get_all(Map, Keys)->
    lager:debug("~p: get_all (~p, ~p).",[?MODULE, Map, Keys]),
    F = fun(Key, {Hits, Misses}) ->
		case jc_store:get(Map, Key) of
		    {ok, #key_to_value{value=Value}} ->
			{[{Key, Value}| Hits], Misses};
		    {ok, jc_miss} ->
			{Hits, [Key|Misses]}
		end
	end,
    {ok, trans_execute(fun() -> lists:foldl(F, {[],[]}, Keys) end)}.



%% -----------------------------------------------------------------------------
%% @doc Return all the keys for a given map.
%% 
-spec key_set(Map::map_name()) -> {ok, Result::list(key())}.

key_set(Map) ->
    lager:debug("~p: key_set for ~p.",[?MODULE, Map]),
    F = fun() -> jc_store:key_set(Map) end,
    trans_execute(F).



%% -----------------------------------------------------------------------------
%% @doc Return all values in the given map.
%%
-spec values(Map::map_name()) ->  {ok, [value()]}.

values(Map) ->
    lager:debug("~p: values for ~p.",[?MODULE, Map]),
    {ok, Recs} = trans_execute(fun() -> jc_store:get_map(Map) end),
    F = fun(#key_to_value{value=Value}, Acc) -> [Value | Acc] end,
    {ok, lists:foldl(F, [], Recs)}.


%% -----------------------------------------------------------------------------
%% @doc Return the Values where the value matches the supplied criteria.
%% Assumes the the criteria is a string in the form of "a.b.c=true", where 
%% a.b.c is dot-path consisting of dot-separated JSON object-keys or JSON array
%% indexes: "bed.id=10" or "bed.id.2.type.something=\"stringvalue\"".
%%
-spec values_match(map_name(), Criteria::string()) ->  {ok, [{key(), value()}]}.

values_match(Map, Criteria) -> 
    lager:debug("~p: values_match ~p with  ~p.", [?MODULE, Map, Criteria]),
    Fun = fun(_M, K, V, Acc) -> [{K, V} | Acc] end,
    fun_match(Map, Criteria, Fun).


    

		
%% =============================================================================
%% Utility functions
%% =============================================================================


%% -----------------------------------------------------------------------------
%% Convert the criteria, "a.2.d=1" to {Paths, Test} = {{<<"a">>,2,<<"d">>},1} 
%% and then ask jc_store to to apply the supplied function for the cache items
%% whose JSON value at the path equals the Test.
%%
-spec fun_match(map(), string(), fun()) -> [term()].

fun_match(Map, Criteria, Fun) ->
    case path2tuple(Criteria) of 
	error ->
	    lager:warning("~p: could not parse criteria ~p.", 
			  [?MODULE, Criteria]),
	    {ok, []};
	{Paths, Test} ->
	    Trans = fun()->jc_store:fun_match(Map, Paths, Test, Fun) end,
	    {ok, trans_execute(Trans)}
    end.


%% -----------------------------------------------------------------------------
%% Execute F in the context of a transaction, if F is not already
%% executing in the context of a transaction -- Avoids nested transactions
%% 
-spec trans_execute(fun(() -> any())) -> any().

trans_execute(F) ->
    case mnesia:is_transaction() of
	true ->     F();
	false ->    mnesia:sync_dirty(F)
    end.



%% ----------------------------------------------------------------------------
%% convert criteria into a tuple path and an rvalue.
%%
path2tuple(Criteria) ->
    try 
	C = case is_binary(Criteria) of
		true -> binary_to_list(Criteria);
		false -> Criteria
	    end,
	{LValue, Test} = split(C),
	
	{make_ej_path(LValue), decode(Test)}
    catch
	_:_ -> error
    end.
    

split(Criteria)->
    [LValue, Test] = string:tokens(Criteria, "="),
    SLValue = string:strip(LValue),
    STest = string:strip(Test),
    {SLValue, STest}.


%% jsonx NIF doesnt compile on windows so use erlang jsone library

-ifdef('NO_NIF').
-define(JSON_DECODE(X), jsone:decode(X)).
-else.
-define(JSON_DECODE(X), jsonx:decode(X)).
-endif.

decode(Value) ->
    try ?JSON_DECODE(list_to_binary(Value))
    catch 
	_:_-> throw(error)
    end.



% ------------------------------------------------------------------------------
% The Ej library navigates a decoded JSON structure via tuple descibing the 
% path to traverse. Elements are either an object key (string) or an index 
% into an array (number). 
%
-spec make_ej_path(string()) -> tuple().

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
