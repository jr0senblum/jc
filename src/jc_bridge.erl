%%% ----------------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2014, 
%%% @doc
%%% Receives requests of the form {CallerPid::pid(), Message::tuple()}, 
%%% spawns a process which calls the appropriate module:fn and returns the 
%%% result to the caller. Primarily used by the Java node (via JInterface) to
%%% exercise the cache.
%%%
%%% Since this server is the last thing started by the supervisor, it notifys
%%% all other node's jc_psub that it is up signalaing the node's readiness to act
%%% as a node cache.
%%% @end
%%% Created : 28 Oct 2014 by  <Jim Rosenblum>
%%% ----------------------------------------------------------------------------
-module(jc_bridge).

-behaviour(gen_server).


%% Module API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).


-record(jc_bridge_state, {}).



%%% ----------------------------------------------------------------------------
%%% Module API
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @doc Starts the server
%%
-spec start_link() -> ignore | {error, _} | {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Initializes the server, let all jc_psub's know that this node is ready.
%%
-spec init([]) -> {ok, #jc_bridge_state{}}.

init([]) ->
    Nodes = application:get_env(jc, cache_nodes,[node()]),
    lager:info("~p: letting ~p know that this node is ready.", 
	       [?MODULE, Nodes]),

    gen_server:abcast(Nodes, jc_psub, {jc_bridge_up, node()}),
    lager:info("~p: up.", [?MODULE]),
    {ok, #jc_bridge_state{}}.


%% -----------------------------------------------------------------------------
%% @private Handling call messages
%%
-spec handle_call(term(), {pid(), _}, #jc_bridge_state{}) -> 
			                        {reply, ok, #jc_bridge_state{}}.

handle_call(Request, _From, State) ->
    lager:warning("~p: unexpected call request: ~p.", [?MODULE, Request]),
    {reply, ok, State}.


%% -----------------------------------------------------------------------------
%% @private Handling cast messages.
%%
-spec handle_cast(any(), #jc_bridge_state{}) -> {noreply, #jc_bridge_state{}}.

handle_cast(Msg, State) ->
   lager:warning("~p: unexpected cast request: ~p.", [?MODULE, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Handling info messages:
%% Execute the requested j_cache operation and return the answer to requester.
%%
-spec handle_info(any(), #jc_bridge_state{}) -> {noreply, #jc_bridge_state{}}.

handle_info({From, {map_subscribe, Map, Key, Ops}}, State) ->
    _Pid = spawn(fun()->From ! jc_psub:map_subscribe(From, Map, Key, Ops) end),
    {noreply, State};

handle_info({From, {topic_subscribe, Topic, Value}}, State) ->
    _Pid = spawn(fun()->From ! jc_psub:topic_subscribe(From, Topic, Value) end),
    {noreply, State};

handle_info({From, {map_unsubscribe, Map, Key, Ops}}, State) ->
    _Pid = spawn(fun()->From ! jc_psub:map_unsubscribe(From, Map, Key, Ops) end),
    {noreply, State};

handle_info({From, {topic_unsubscribe, Topic, Value}}, State) ->
    _Pid = 
	spawn(fun()->From ! jc_psub:topic_unsubscribe(From, Topic, Value) end),
    {noreply, State};

handle_info({From, {topic_event, Topic, Value}}, State) ->
    _Pid = spawn(fun()->From ! jc_psub:topic_event(Topic, Value) end),
    {noreply, State};

handle_info({From, {node_topic_sub}}, State) ->
    _Pid = spawn(fun() -> From ! jc_psub:topic_subscribe(From, 
							 jc_node_events, 
							 any)
		 end),
    {noreply, State};

handle_info({From, {node_topic_unsub}}, State) ->
    _Pid = spawn(fun()->From ! jc_psub:topic_unsubscribe(From, 
							 jc_node_events, 
							 any) 
		 end),
    {noreply, State};


handle_info({From, {set_max_ttl, Map, Secs}}, State) ->
    _Pid = 
	spawn(fun()->From ! jc_eviction_manager:set_max_ttl(Map, Secs) end),
    {noreply, State};

handle_info({From, {get_max_ttls}}, State) ->
    _Pid = 
	spawn(fun()->From ! jc_eviction_manager:get_max_ttls() end),
    {noreply, State};


handle_info({From, {put, Map, Key, Value}}, State) ->
    _Pid = spawn(fun() ->From ! jc:put(Map, Key, Value) end),
    {noreply, State};

handle_info({From, {put_s, Map, Key, Value, Seq}}, State) ->
    _Pid = spawn(fun() ->From ! jc_s:put(Map, Key, Value, Seq) end),
    {noreply, State};

handle_info({From, {put, Map, Key, Value, Ttl}}, State) ->
    _Pid = spawn(fun() -> From ! jc:put(Map, Key, Value, Ttl) end),
    {noreply, State};

handle_info({From, {put_s, Map, Key, Value, Ttl, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:put(Map, Key, Value, Ttl, Seq) end),
    {noreply, State};

handle_info({From, {put_all, Map, List}}, State) ->
    _Pid = spawn(fun() -> From ! jc:put_all(Map, List) end),
    {noreply, State};

handle_info({From, {put_all, Map, List, Ttl}}, State) ->
    _Pid = spawn(fun() -> From ! jc:put_all(Map, List, Ttl) end),
    {noreply, State};

handle_info({From, {put_all_s, Map, List, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:put_all(Map, List, Seq) end),
    {noreply, State};

handle_info({From, {put_all_s, Map, List, Ttl, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:put_all(Map, List, Ttl, Seq) end),
    {noreply, State};


handle_info({From, {sequence}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:sequence() end),
    {noreply, State};

handle_info({From, {sequence, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:sequence(Map) end),
    {noreply, State};

handle_info({From, {clear, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc:clear(Map) end),
    {noreply, State};

handle_info({From, {evict_map_since, Map, Secs}}, State) ->
    _Pid = spawn(fun() -> From ! jc:evict_map_since(Map, Secs) end),
    {noreply, State};

handle_info({From, {evict, Map, Key}}, State) ->
    _Pid = spawn(fun() -> From ! jc:evict(Map, Key) end),
    {noreply, State};
handle_info({From, {evict_s, Map, Key, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:evict(Map, Key, Seq) end),
    {noreply, State};

handle_info({From, {evict_match, Map, Criteria}}, State) ->
    _Pid = spawn(fun() -> From ! jc:evict_match(Map, Criteria) end),
    {noreply, State};

handle_info({From, {evict_match_s, Map, Criteria, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:evict_match(Map, Criteria, Seq) end),
    {noreply, State};

handle_info({From, {evict_all_match, Criteria}}, State) ->
    _Pid = spawn(fun() -> From ! jc:evict_all_match(Criteria) end),
    {noreply, State};

handle_info({From, {evict_all_match_s, Criteria, Seq}}, State) ->
    _Pid = spawn(fun() -> From ! jc_s:evict_all_match(Criteria, Seq) end),
    {noreply, State};

handle_info({From, {flush}}, State) ->
    _Pid = spawn(fun() ->From ! jc:flush() end),
    {noreply, State};

handle_info({From, {flush, silent}}, State) ->
    _Pid = spawn(fun() ->From ! jc:flush(silent) end),
    {noreply, State};

handle_info({From, {remove_items, Map, Key}}, State) ->
    _Pid = spawn(fun() ->From ! jc:remove_items(Map, Key) end),
    {noreply, State};

handle_info({From, {remove_items_s, Map, Keys, Seq}}, State) ->
    _Pid = spawn(fun() ->From ! jc_s:remove_items(Map, Keys, Seq) end),
    {noreply, State};

handle_info({From, {contains_key, Map, Key}}, State) ->
    _Pid = spawn(fun() -> From ! jc:contains_key(Map, Key) end),
    {noreply, State};

handle_info({From, {get, Map, Key}}, State) ->
    _Pid = spawn(fun() ->From ! jc:get(Map, Key) end),
    {noreply, State};

handle_info({From, {get_all, Map, Key}}, State) ->
    _Pid = spawn(fun() -> From ! jc:get_all(Map, Key) end),
    {noreply, State};

handle_info({From, {key_set, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc:key_set(Map) end),
    {noreply, State};

handle_info({From, {values, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc:values(Map) end),
    {noreply, State};

handle_info({From, {values_match, Map, Criteria}}, State) ->
    _Pid = spawn(fun() -> From ! jc:values_match(Map, Criteria) end),
    {noreply, State};

handle_info({From, {cache_nodes}}, State) ->
    _Pid = spawn(fun() -> From ! jc:cache_nodes() end),
    {noreply, State};

handle_info({From, {cache_size}}, State) ->
    From ! jc:cache_size(),
    {noreply, State};

handle_info({From, {map_size, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc:map_size(Map) end),
    {noreply, State};

handle_info({From, {maps}}, State) ->
    _Pid = spawn(fun() -> From ! jc:maps() end),
    {noreply, State};

handle_info({From, {up}}, State) ->
    _Pid = spawn(fun() -> From ! jc:up() end),
    {noreply, State};


handle_info({From, {start_indexing, Map, Path}}, State) ->
    _Pid = spawn(fun() -> From ! jc_store:start_indexing(Map, Path) end),
    {noreply, State};

handle_info({From, {stop_indexing, Map, Path}}, State) ->
    _Pid = spawn(fun() -> From ! jc_store:stop_indexing(Map, Path) end),
    {noreply, State};

handle_info({From, {indexes}}, State) ->
    _Pid = spawn(fun() -> From ! jc_store:indexes() end),
    {noreply, State};

handle_info({From, {indexes, Map}}, State) ->
    _Pid = spawn(fun() -> From ! jc_store:indexes(Map) end),
    {noreply, State};


			  
handle_info(Info, State) ->
    lager:warning("~p: unrecognized handle_info message: ~p.",
		  [?MODULE,Info]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Called by a gen_server when it is about to  terminate.
%%
-spec terminate(any(), #jc_bridge_state{}) -> any().

terminate(_Reason, _State) ->
    ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed
%%
-spec code_change(term(), #jc_bridge_state{}, any()) -> {ok, #jc_bridge_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

