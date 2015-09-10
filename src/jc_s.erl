%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2015, Jim Rosenblum
%%% @doc This module wraps the mnesia-interacting, lower-level functions
%%% implemented in {@link jc_store. jc_store} and utilizes a singleton
%%% gen_server to provide support for serializing opperations.
%%%
%%% A sequence parameter is used to disallow PUT, EVICT and REMOVE
%%% operations whose sequence parameter is less than the highest, map-specific,
%%% sequence numbers seen. 
%%%
%%% s_jc can be called directly by Erlang clients; or,
%%% Java node -> JInterface -> {@link jc_bridge. jc_bridge} -> s_jc
%%% 
%%% @version {@version}
%%% @end
%%% Created : 16 December 2011 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(jc_s).


% Put Functions
-export([put/4, put/5,
	 put_all/3, put_all/4]).

% Delete Functions
-export([evict/3, 	 
	 evict_all_match/2,
	 evict_match/3, 
	 remove_items/3]).

% Meta Functions
-export([sequence/1, sequence/0]).

 % Record and type definitions.
-include("../include/records.hrl").   


-type trx_ret() :: {error, badarg | out_of_seq | term()}.


-define(INFINITY, 0).
-define(NO_SEQ, -1).
-define(VALID(X), is_integer(X) andalso (X >= 0)).



%% =============================================================================
%% META API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Return the sequence number associated with the given map.
%%
-spec sequence(map()) -> {ok, [{atom, non_neg_integer()}]}.

sequence(Map) ->
    {ok, sequence_for(Map)}.


%% -----------------------------------------------------------------------------
%% @doc Return a sorted list of {map, sequence numbers} for each map.
%%
-spec sequence() -> {ok, [{atom, non_neg_integer()}]}.

sequence() ->
    {ok, sequence_for(all)}.

    


%% =============================================================================
%% PUT API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Put the entry into the cache with a TTL of infinity using the sequence
%% number to ensure serialized operations.
%%
-spec put(map_name(), key(), value(), jc_sequence:seq()) -> 
		 {ok, {key, key()}} | trx_ret().


put(Map, Key, Value, Seq) when ?VALID(Seq) ->
    put(Map, Key, Value, ?INFINITY, Seq);

put(_Map, _Key, _Value, _Seq) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Put the entry into the cache with the TTL using the sequence number
%% to ensure serialized operations.
%%
-spec put(map_name(), key(), value(), ttl(), jc_sequence:seq()) -> 
		 {ok, {key, key()}} |
						       trx_ret().


put(Map, Key, Value, TTL, Seq) when ?VALID(TTL) andalso ?VALID(Seq)-> 
    lager:debug("~p: put {~p, ~p} with TTL: ~p and seq: ~p.", 
		[?MODULE, Map, Key, TTL, Seq]),

    F = fun() -> 
		case test_set(Map, Seq) of
		    true -> do_put(Map, Key, Value, TTL);
		    false -> {error, out_of_seq}
		end
	end,
    Message = "~p: out of sequence put (~p, ~p) ~p.",
    trans_execute(F, Message, [?MODULE, Map, Key, Seq]);


put(_M, _K, _V, _T, _S)  ->
    {error, badarg}.


% Need to do the actual put from a number of functions, so pull it out to reduce
% nested transactions.
%
do_put(Map, Key, Value, TTL) ->
    Ref = make_ref(),
    {ok, {put, Ref}} = jc_store:put(Map, Key, Value, TTL, Ref),
    {ok, {key, Key}}.
    


%% -----------------------------------------------------------------------------
%% @doc Put all the {K,V} tuples contained in the list with a TTL of infinity.
%% Use the sequence number to ensure serialized operations. Return the number
%% of successes.
%%
-spec put_all(map_name(), list({key(), value()}), jc_sequence:seq()) -> 
		     {ok, non_neg_integer()} | trx_ret().


put_all(Map, KVList, Seq) when ?VALID(Seq) -> 
    put_all(Map, KVList, ?INFINITY, Seq);

put_all(_M, _K, _S) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Put all the {K,V} pairs contained in the list using the supplied TTL. 
%% Use the sequence number to ensure serialized operations. Return the number
%% of successes.
%%
-spec put_all(map_name(), list({key(), value()}), ttl(), jc_sequence:seq()) -> 
		     {ok, non_neg_integer()} | trx_ret().


put_all(Map, KVList, TTL, Seq) when ?VALID(TTL) andalso ?VALID(Seq) ->
    lager:debug("~p: put_all for map: ~p with TTL: ~p and seq: ~p.", 
		[?MODULE, Map, TTL, Seq]),
    F = fun() -> 
		case test_set(Map, Seq) of
		    true -> 
			[do_put(Map, Key, Value, TTL) || {Key, Value} <- KVList];
		    false -> {error, out_of_seq}
		end
	end,
    M = "~p: out of sequence put_all Map: ~p, and Seq: ~p.",
    case trans_execute(F, M, [?MODULE, Map, Seq]) of
	{error, _} = E -> 
	    E;
	Results -> 
	    {ok, length([K || {ok, {key, K}} <- Results])}
    end;


put_all(_M, _K, _T, _S) ->
    {error, badarg}.



%% -----------------------------------------------------------------------------
%% @doc Evict {@link map_name(). Map}, {@link key(). Key} if sequence is 
%% greater than or equal to the last seen sequence.
%
-spec evict(map_name(), key(), jc_sequence:seq()) -> ok | trx_ret().

evict(Map, Key, Seq) ->
    lager:debug("~p: evict map:~p, key: ~p, and seq: ~p.).", 
		[?MODULE, Map, Key, Seq]),

    F = fun() ->
		case test_set(Map, Seq) of
		    true ->
			jc_store:evict(Map, Key);
		    false -> {error, out_of_seq}
		end
	end,
    M = "~p: out of sequence evict {~p, ~p}, seq: ~p.",

    case trans_execute(F, M, [?MODULE, Map, Key, Seq]) of
	{error, _} = E ->
	    E;
	_Result ->
	    ok
    end.



%% -----------------------------------------------------------------------------
%% @doc Evict Map/Key from the cache for Key's whose value matches the criteria
%% if not out of sequence.
%% Assumes the the criteria is a string in the form of "a.b.c=true", where 
%% a.b.c is dot-path consisting of dot-separated JSON object-keys or JSON array
%% indexes: "bed.id=10" or "bed.id.2.type.something=\"stringvalue\"".
%%
-spec evict_match(map_name(), Criteria::string(), jc_sequence:seq()) ->  
			 ok | trx_ret().

evict_match(Map, Criteria, Seq) when ?VALID(Seq) ->
    lager:debug("~p: evict_match with map: ~p, criteria: ~p and seq: ~p.", 
		[?MODULE, Map, Criteria, Seq]),

    F = fun() ->
		case test_set(Map, Seq) of
		    true ->
			do_evict_match(Map, Criteria);
		    false -> {error, out_of_seq}
		end
	end,
    M = "~p: out of sequence evict_match. Map: ~p, Criteria: ~p, and seq ~p.",
    case trans_execute(F, M, [?MODULE, Map, Criteria, Seq]) of
	{error, _} = E ->
	    E;
	_Result ->
	    ok
    end;

evict_match(_M, _C, _S) ->
    {error, badarg}.


% Need to do the actual evict_match from a number of functions, so pull it
% out to reduce nested transactions.
%
do_evict_match(Map, Criteria) ->
    Fun = fun(M, K, _, Acc) ->  jc_store:evict(M, K), Acc end,
    jc:fun_match(Map, Criteria, Fun),
    ok.



%% -----------------------------------------------------------------------------
%% @doc Call {@link evict_match/2} for each Map with the given sequence number.
%% Assumes the the criteria is a string in the form of "a.b.c=true", where 
%% a.b.c is dot-path consisting of dot-separated JSON object-keys or JSON array
%% indexes: "bed.id=10" or "bed.id.2.type.something=\"stringvalue\"".
%%
-spec evict_all_match(Criteria::string(), jc_sequence:seq()) ->  
			     ok | trx_ret().

evict_all_match(Criteria, Seq) when ?VALID(Seq) ->
    lager:debug("~p: evict_all_match with ~p and seq ~p:.", 
		[?MODULE, Criteria, Seq]),
    _ = [evict_match(M, Criteria, Seq) || M <- jc:maps()],
    ok;

evict_all_match(_C, _S) ->
    {error, badarg}.

    


%% -----------------------------------------------------------------------------
%% @doc Evict all K's, return all {K, V} pairs that were found. Enforces that
%% the sequence number is greater than what has been seen.
%%
-spec remove_items(Map::map_name(), Keys::[key()], jc_sequence:seq()) -> 
			  {ok, [{key(), value()}]} | trx_ret().

remove_items(Map, Keys, Seq) when ?VALID(Seq) ->
    lager:debug("~p: remove_items (~p, ~p, ~p).",[?MODULE, Map, Keys, Seq]),

    F = fun(Key, Acc) ->
		case jc_store:get(Map, Key) of
		    {ok, #key_to_value{value = Value}} ->
			jc_store:evict(Map, Key),
			[{Key, Value} | Acc];
		    {ok, jc_miss} ->
			Acc
		end
	end,
    
    F2 = fun() ->
		 case test_set(Map, Seq) of
		     true ->     
			 lists:foldl(F, [], Keys);
		     false ->
			 {error, out_of_seq}
		 end
	 end,
    M = "~p: out of sequence remove_items. Map: ~p, Keys: ~p, Seq: ~p.",
    case trans_execute(F2, M, [?MODULE, Map, Keys, Seq]) of
	{error, _} = E ->
	    E;
	Result ->
	    {ok, Result}
    end;

remove_items(_M, _K, _S) ->
    {error, badarg}.




%% -----------------------------------------------------------------------------
%% If new sequence is >= to existing one, bump the sequence number and return 
%% true.
%%
test_set(Map, Seq) ->
    try jc_sequence:test_set(Map, Seq) 
    catch
	exit:{timeout, _R} ->
	    lager:warning("~p: call to jc_sequence timed out. Retrying.",[?MODULE]),
	    timer:sleep(100),
	    test_set(Map, Seq);
	C:E ->
	    lager:warning("~p: call to jc_sequence failed: ~p:~p. Retrying.",
			  [?MODULE, C, E]),
	    timer:sleep(100),
	    test_set(Map, Seq)

    end.


%% -----------------------------------------------------------------------------
%% Return sorted [{map, seq_no}] per the input parameter. Hit the table 
%% directly, no need to use the singleton, gen_server.

sequence_for(all) ->
    case mnesia:dirty_match_object(seq, mnesia:table_info(seq, wild_pattern)) of
	[] -> 
	    [];
	Records ->
	    lists:sort([{R#seq.map, R#seq.seq_no} || R <- Records])
    end;

sequence_for(Map) ->
    case mnesia:dirty_read({seq, Map}) of
	[] -> 
	    not_exist;
	[#seq{seq_no = No}] ->
	    No
    end.


		 
%% -----------------------------------------------------------------------------
%% Execute F in the context of a transaction, if F is not already executing in 
%% the context of a transaction -- Avoids nesting.
%% 
-spec trans_execute(fun(() -> any()), string(), [term()]) -> trx_ret().

trans_execute(F, Message, Params) ->
    case mnesia:is_transaction() of
	true ->     F();
	false ->    
	    case mnesia:sync_dirty(F) of
		{error, out_of_seq}=SeqError -> 
		    lager:warning(Message, Params),
		    SeqError;
		{exit, R} -> 
		    {error, R};
		Result ->
		    Result
	    end
    end.





