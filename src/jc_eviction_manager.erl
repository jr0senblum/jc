%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2015, Jim Rosenblum
%%% @doc jc_eviction_manager 
%%% 1: Implements item-level ttl by spying on the cache table, creating and 
%%% deleting TTL timers as indicated by the ttl attribute of the cache entry.
%%% A unique reference is assocateid with a cached item, and a TTL timer 
%%% has this unique reference as part of its sate. When the timer goes off, 
%%% it evicts using this cache-item reference. This ensures that the timer never
%%% evicts anything but the correct'version' of an item. If an item is updated 
%%% (a 'Put' with a Map and Key of an existing value), it will get a new 
%%% referenceso the old TTL will not evict the updated version even if it goes
%%% off before it can be cancelled.
%%% 2: Implements a map-based ttl (max_ttl) by using a timer to periodically
%%% evict any cache items whose create time is older than the configured value
%%% for a given map.
%%% @end
%%% Created : 25 Oct 2011 by Jim Rosenblum
%% -----------------------------------------------------------------------------
-module(jc_eviction_manager).

-behaviour(gen_server).


%% API
-export([start_link/0]).

% Api to start, stop and extend item-level TTL timers.
-export([add_timer/2, 
	 cancel_timer/1]).

% Api to set and retrieve Map-level max TTLs.
-export([set_max_ttl/2,
	 get_max_ttls/0]).

% gen_server callbacks.
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2,
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).


% Internal function used in spawn.
-export([handle_max_ttl/1]).


 % Table and record definitions.
-include("../include/records.hrl").   


-define(SERVER, ?MODULE).
-define(INFINITY, 0).
-define(MAX_TTL_JOB_SECS, 120).


-record(jc_evict_mngr_state, {}).



%%% ============================================================================
%%% Module API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc Create a TTL timer for the cache entry with the supplied reference. A 
%% 0 TTL is ignored as it is the equivalent of infinity. 
%%
-spec add_timer(ttl(), rec_ref()) -> ok.

add_timer(?INFINITY, _KVRecRef) ->
    ok;

add_timer(TTLSecs, KVRecRef) ->
    lager:debug("~p: adding TTL for the record with reference ~p.",
		[?MODULE, KVRecRef]),
    TTLMsecs = TTLSecs * 1000,
    Ref = erlang:send_after(TTLMsecs, ?MODULE, {evict, KVRecRef}),
    mnesia:dirty_write(#ttl{key       = KVRecRef, 
			    timer_ref = Ref, 
			    ttl_secs  = TTLSecs}),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Cancel the timer for the cache entry with the supplied reference.
%% Returns the time left before it would have fired, in milliseconds or false
%% if the timer no longer exists.
%%
-spec cancel_timer(RecRef::rec_ref()) -> Time::non_neg_integer() | false.

cancel_timer(RecRef) ->
    case mnesia:dirty_read({ttl, RecRef}) of 
	[#ttl{timer_ref = Timer} = Rec] ->
	    lager:debug("~p: cancelling TTL for record ~p and timer ~p.",
			[?MODULE, RecRef, Timer]),
	    Result = erlang:cancel_timer(Timer),
	    mnesia:dirty_delete_object(Rec),
	    Result;
	[] -> 
	    false
    end.


%% -----------------------------------------------------------------------------
%% @doc Create, change or delete the max TTL associated with a map. 0 seconds 
%% will remove the max_ttl for the map alltogether. Existing values are 
%% overwritten.
%%
-spec set_max_ttl(Map::map_name(), Secs::seconds()) -> ok | {error, badarg}.

set_max_ttl(Map, ?INFINITY) ->
    lager:debug("~p: removing max ttl from map ~p.", [?MODULE, Map]),
    mnesia:dirty_delete({max_ttl, Map}),
    ok;

set_max_ttl(Map, Secs) when is_integer(Secs) andalso Secs > 0->
    lager:debug("~p: updating max ttl for map ~p to ~p.", [?MODULE, Map, Secs]),
    mnesia:dirty_write(#max_ttl{map = Map, ttl_secs = Secs}),
    ok;

set_max_ttl(_Map, Bad) ->
    lager:warning("~p: bad value sent to set_max_ttl: ~p.", [?MODULE, Bad]),
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Return [{Map, TTLSecs}].
%% 
-spec get_max_ttls() -> {ttls, [{Map::map_name(), Secs::seconds()}]}.

get_max_ttls()->
    Trans = fun() ->
		    mnesia:foldl(fun(#max_ttl{map = M, ttl_secs = S}, Acc) -> 
					 [{M, S}| Acc]
				 end,
				 [],
				 max_ttl)
	    end,
    {ttls, trans_execute(Trans)}.



%%% ============================================================================
%%% Module API 
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc Starts the server and links the caller to it.
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private Initialize the server by subscribing to key_to_value table changes.
%% and seeting up the timer for max_ttl evicts.
%%
-spec init([]) -> {ok, #jc_evict_mngr_state{}}.

init([]) ->
    get_default_max_ttls(),
    create_max_ttl_timer(),
    mnesia:subscribe({table, key_to_value, detailed}),
    lager:info("~p: up.", [?SERVER]),
    {ok, #jc_evict_mngr_state{}}.


get_default_max_ttls() ->
    MaxTTLMaps = application:get_env(jc, max_ttl_maps, []),
    [set_max_ttl(Map, Secs)|| {Map, Secs} <- MaxTTLMaps].

    
create_max_ttl_timer() ->
    JobSecs = application:get_env(jc, max_ttl_job_secs, ?MAX_TTL_JOB_SECS),
    create_max_ttl_job(JobSecs).



%% -----------------------------------------------------------------------------
%% @private Hande call messages.
%%
-spec handle_call(term(), {pid(), _}, #jc_evict_mngr_state{}) ->
    			                     {reply, ok, #jc_evict_mngr_state{}}.

handle_call(Request,  _From, State) ->
    lager:warning("~p: unexpected call request: ~p.",[?SERVER, Request]),
    {reply, ok, State}.



%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_evict_mngr_state{}) -> 
			 {noreply, #jc_evict_mngr_state{}}.

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.",[?SERVER, Msg]),
    {noreply, State}.



%% -----------------------------------------------------------------------------
%% @private Handle info messages: eviction and timer create/deteles due to
%% key_to_value table changes.
%%
-spec handle_info(any(), #jc_evict_mngr_state{}) -> 
			                      {noreply, #jc_evict_mngr_state{}}.

handle_info({mnesia_table_event, {delete, key_to_value, _What,
				  [Rec], _Trx}}, State) ->

    #key_to_value{map = M, key = K, ref = RecRef, ttl_secs = TTL} = Rec, 
    case TTL of
	?INFINITY ->
	    ok;
	_TTL -> 
	    lager:debug("~p: cancelling timer for {~p, ~p, ~p}.",
			[?SERVER, M, K, RecRef]),
	    cancel_timer(RecRef)
    end,
    {noreply, State};
handle_info({mnesia_table_event, {delete, key_to_value, _What,
				  [], _Trx}}, State) ->
    {noreply, State};

handle_info({mnesia_table_event, {write, key_to_value, 
				  Rec,
				  OldRecs, _Trx}}, State) ->

    #key_to_value{map = M, key = K, ref = RecRef, ttl_secs = TTL} = Rec,
    case TTL of
	?INFINITY ->
	    ok;
	_TTL ->
	    lager:debug("~p: adding timer for {~p, ~p, ~p}.",
			[?SERVER, M, K, RecRef]),
	    add_timer(TTL, RecRef)
    end,
    case OldRecs of
	[#key_to_value{ref=OldRef, ttl_secs=OldTTL}] when OldTTL /= ?INFINITY ->
	    lager:debug("~p: cancelling timer, record replaced {~p, ~p, ~p}.",
			[?SERVER, M, K, RecRef]),
	    cancel_timer(OldRef);
	_ ->
	    ok
    end,
    {noreply, State};


% flush delete's the table form the scheema
handle_info({mnesia_table_event, {delete, schema, {schema, _}, _What, _Trx}}, State) ->
    {noreply, State};

% flush adds the table to the sceema
handle_info({mnesia_table_event, {write, schema, {schema, _, _What}, _Rec, _Trx}}, State) ->

    {noreply, State};


handle_info({max_ttl_evict, Secs}, State) ->
    lager:debug("~p: initiating max_ttl evictions.",[?SERVER]),
    _ = spawn(fun()-> jc_eviction_manager:handle_max_ttl(Secs) end),
   {noreply, State};

handle_info({evict, RecRef}, State) ->
    lager:debug("~p: eviction request for record with reference ~p.",
		[?SERVER, RecRef]),
    handle_evict(RecRef),
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("~p: unexpected info message: ~p.",[?SERVER, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_evict_mngr_state{}) -> any().

terminate(_Reason, _State) ->
    ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_evict_mngr_state{}, any()) -> 
			 {ok, #jc_evict_mngr_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private For any map that is set up with a max ttl, delete all members of
%% the Map whose create date is older than the defined age in seconds.
%% So that handle_info can return right away, it spawns a process to run this
%% function.
%%
-spec handle_max_ttl(Secs::seconds()) -> no_return().

handle_max_ttl(Secs) ->
    CullFn = fun(#max_ttl{map = Map, ttl_secs = Max_Secs}, Acc) ->
		lager:debug("~p: max_ttl evicting from map ~p, older than ~p.", 
			    [?MODULE, Map, Secs]),
		jc:evict_map_since(Map, Max_Secs),
		Acc
	end,
    Trans = fun() -> mnesia:foldl(CullFn, [], max_ttl) end,
    trans_execute(Trans),
    create_max_ttl_job(Secs).


% Set a timer to call this gen_server back with the max_ttl_evict message.
create_max_ttl_job(JobSecs)->
    erlang:send_after(JobSecs*1000, ?MODULE, {max_ttl_evict, JobSecs}).


%% -----------------------------------------------------------------------------
%% @private Evict key_to-value record - item-level TTL.
%%
handle_evict(RecRef) ->
    jc:delete_record_by_ref(RecRef),
    mnesia:dirty_delete({ttl, RecRef}).



%% -----------------------------------------------------------------------------
%% Execute F in the context of a transaction, if F is not already
%% executing in the context of a transaction -- Avoids nested transactions
%% 
-spec trans_execute(fun(() -> any())) -> any().

trans_execute(F) ->
    case mnesia:is_transaction() of
	true ->     F();
	false ->    mnesia:async_dirty(F)
    end.
