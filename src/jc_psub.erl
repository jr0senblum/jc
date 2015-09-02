%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2015, Jim Rosenblum
%%% @doc jc_psub 1) allows clients to subscribe to j_cache changes; 2) notifies 
%%% clients of changes to j_cache items; and 3) allows clients to subscribe to,
%%% and broadcast, topic events.
%%%
%%% Subscriptions are a local afair so if a node goes down, clients that were
%%% using that node's jc_psub will need to re-subscribe.
%%% 
%%% Mnesia LOCAL tables, ps_sub and ps_client, are used:
%%%
%%% ps_sub maps a subscription to a list of listening Pids
%%% ps_client maps a unique, subscriber's pid to a reference or link.
%%%
%%% The server subscribes to mnesia key_to_value, table changes thereby allowing
%%% it to notice cache changes, construct the corresponding subscription
%%% message and look for relevant subscribers to whom to send a message.
%%%
%%% @end
%%% Created : 16 Sep 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc_psub).

-behaviour(gen_server).


% Module API for starting and stopping the change_relay server.
-export([start_link/0]).


% Pub/Sub API.
-export([map_subscribe/4,
	 map_unsubscribe/4,
	 topic_event/2,
	 topic_subscribe/3, 
	 topic_unsubscribe/3]).

% Fns returning various statistics.
-export([client_count/0, load/0]).

% Gen_server callbacks.
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, 
	 code_change/3]).


-include("../include/records.hrl").


-define(SERVER,       ?MODULE). 
-define(DEADBEATS_MS, 3600000).       % Length of time to check for deadbeats.


% Records used to represent map and topic subscriptions.
-record(map_sub, {map              :: map_name(),
		  key = any        :: key() | any,
		  operation = any  :: write|delete|any}).

-record(topic_sub, {topic          :: atom(),
		    value = any    :: value() | any}).


% Server state. Monitored is the list of nodes at which jc_sup is being 
% monitored - don't want multiple monitors so we maintain a list of them.
-record(jc_psub_state, {evict_deadbeats_ms = ?DEADBEATS_MS :: non_neg_integer(),
			nodes              = []            :: list(node()),
			monitored          = []            :: list(node())}).



%%% ============================================================================
%%% Module API 
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc Starts the jc_psub server and links the caller to it.
%% 
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%% ============================================================================
%%% Pub/Sub API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc Adds a subscription on behalf of the process indicated by the supplied 
%% Pid. 
%%
-spec map_subscribe(pid(), map_name(), key()|any, write | delete | any) ->  
			                                   ok | {error, badarg}.

map_subscribe(Client, Map, Key, Ops) when is_pid(Client)->
    SubSpec = #map_sub{map = Map, key = Key, operation = Ops},
    gen_server:call(?SERVER, {subscribe, Client, SubSpec});

map_subscribe(_C, _M, _K, _O) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Client unsubcribes from notifiction.
%% 
-spec map_unsubscribe(pid(), map_name(),key()|any, write | delete | any) ->
			                                   ok | {error, badarg}.
			 
map_unsubscribe(Client, Map, Key, Ops) when is_pid(Client) ->
    SubSpec = #map_sub{map = Map, key = Key, operation = Ops},
    gen_server:call(?SERVER, {unsubscribe, Client, SubSpec});

map_unsubscribe(_C, _M, _k, _O) ->
    {error, badarg}.



%% -----------------------------------------------------------------------------
%% @doc Push a topic message to the server for broadcast to its local 
%% subscribers. If the event is a node event, then the other jc_psubs will 
%% know about it via their monitoring of foreign jc_sup's, so there is no need 
%% to propagate the event to other nodes' jc_psub servers; else propagate it.
%%
-spec topic_event(Topic::atom(), Value::value()) -> ok.

topic_event(jc_node_events, Value)->
    gen_server:cast(?SERVER, {topic_event, jc_node_events, Value});

topic_event(Topic, Value)->
    gen_server:abcast([node()|nodes()], ?SERVER, {topic_event, Topic, Value}),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Client subscribes to notifications regarding the given Topic.
%%
-spec topic_subscribe(Client::pid(), Topic::atom(), Value::value() | any) -> 
  			                                    ok| {error, badarg}.

topic_subscribe(Client, Topic, Value) when is_pid(Client)->
    TopicSpec = #topic_sub{topic = Topic, value = Value},
    gen_server:call(?SERVER, {subscribe, Client, TopicSpec});

topic_subscribe(_C, _T, _V) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Client unsubscribes to notifications regarding the given Topic.
%%
-spec topic_unsubscribe(Client::pid(), Topic::atom(), Value::value() | any) ->
			                                   ok | {error, badarg}.

topic_unsubscribe(Client, Topic, Value) when is_pid(Client) ->
    TopicSpec = #topic_sub{topic = Topic, value = Value},
    gen_server:call(?SERVER, {unsubscribe, Client, TopicSpec});

topic_unsubscribe(_C, _T, _V) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% Various statistics
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Return an estimate of how much load the server is under. It counts the
%% number of subscriptions + the number of Pids that are currenlty subscribed
%% to anything. Under extreme load this can time-out resulting in an error.
%% 
-spec load() -> non_neg_integer() | {error, any()}.

load() ->
    try gen_server:call(?SERVER, load)
    catch
	_Error:Reason ->
	    lager:warning("~p: load/0 error. ~p.", [?SERVER, Reason]),
	    {error, Reason}
    end.


%% -----------------------------------------------------------------------------
%% @doc Return the number of distinct, subscribing Pids being served by the
%% server. Under extreme load this function can time-out.
%% 
-spec client_count() -> non_neg_integer() | {error, any()}.

client_count() ->
    try gen_server:call(?SERVER, client_count)
    catch
	_Error: Reason ->
	    lager:warning("~p: client_count/0 error. ~p", [?SERVER, Reason]),
	    {error, Reason}
    end.



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private
%% Initializes the server. Set process_flag to trap linked subscriber exits,
%% monitor any jc_sups at already up nodes, schedule the deadbeat_eviction job 
%% and subscribe to relevant mnesia changes to table key_to_value.
%%
-spec init([]) -> {'ok', #jc_psub_state{}}.


init([]) ->
    process_flag(trap_exit, true), 

    Nodes = application:get_env(jc, cache_nodes,[node()]),
    DBMs  = application:get_env(jc, evict_deadbeats_ms, ?DEADBEATS_MS),

    MNodes = monitor_sups(Nodes),
    lager:info("~p: found and monitoring existing supervisors at ~p.", 
	       [?SERVER, MNodes]),

    schedule_deadbeat_eviction(DBMs),    

    mnesia:subscribe({table, key_to_value, detailed}),

    lager:info("~p: up.", [?SERVER]),
    {ok, #jc_psub_state{evict_deadbeats_ms = DBMs, 
			nodes = Nodes, 
			monitored = MNodes}}.


% Monitor existing jc_sup processes at remote nodes, return list of those nodes.
monitor_sups(Nodes) ->
    F = fun(Node, Acc) -> 
		case rpc:call(Node, erlang, whereis, [jc_bridge], 500) of
		    undefined                    -> Acc;
		    {badrpc, _}                  -> Acc;
		    Pid when node(Pid) == node() ->  
			% shouldn't happen because jc_bridge isn't up yet.
                        % regardless, don't monitor yourself
			Acc;
		    Pid when Pid /= self()       -> 
			erlang:monitor(process, {jc_sup, Node}),
			[Node | Acc]
		end
	end,
    lists:foldl(F, [], Nodes).


%% -----------------------------------------------------------------------------
%% @private Handling call messages: subscribe, unsubscribe, load, client_clount.
%%
-spec handle_call(term(), {pid(), _}, #jc_psub_state{}) -> 
			 {reply, ok, #jc_psub_state{}}.

handle_call({subscribe, Client, Subscription}, _From, State) ->
    lager:debug("~p: adding subscription: ~p.",[?SERVER, Subscription]),
    subscribe(Client, Subscription),
    {reply, ok, State};

handle_call({unsubscribe, Client, Subscription}, _From, State) ->
    lager:debug("~p: deleting subscription: ~p.",[?SERVER, Subscription]),
    unsubscribe(Client, Subscription),
    {reply, ok, State};

handle_call(load,  _From, State) ->
    lager:debug("~p: calculating load.",[?SERVER]),
    {reply, calculate_load(), State};

handle_call(client_count,  _From, State) ->
    lager:debug("~p: calculating client_count.",[?SERVER]),
    {reply, mnesia:table_info(ps_client, size), State};

handle_call(Request,  _From, State) ->
    lager:warning("~p: unexpected call request: ~p.",[?SERVER, Request]),
    {reply, ok, State}.


%% -----------------------------------------------------------------------------
%% @private Handling cast messages: topic events and foreign jc_bridge up.
%%
-spec handle_cast(any(), #jc_psub_state{}) -> {noreply, #jc_psub_state{}}.

handle_cast({topic_event, Topic, Value} = Message, State) ->
    lager:debug("~p: topic_event: ~p.",[?SERVER, Message]),
    broadcast_topic_event(Topic, Value),
    {noreply, State};

% after jc_bridge started, it notifies all jc_psubs that it is up, monitor it.
handle_cast({jc_bridge_up, UpNode}, #jc_psub_state{nodes = Nodes, monitored=Ms}=S)->
    case (not lists:member(UpNode, Ms)) andalso lists:member(UpNode, Nodes) of
	true ->
	    lager:info("~p: jc_bridge is up at ~p.", [?SERVER, UpNode]),
	    erlang:monitor(process, {jc_sup, UpNode}),
	    signal_nodeup(UpNode),
	    {noreply, S#jc_psub_state{monitored = [UpNode | Ms]}};
	false ->
	    {noreply, S}
    end;

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.",[?SERVER, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Handle info messages for: subscribing client or Node down; a 
%% foreign jc_sup goes down, evicting deadbeats and key_to_value table changes.
%%
-spec handle_info(any(), #jc_psub_state{}) -> {noreply, #jc_psub_state{}}.

% Monitored jc_sup went down on some node
handle_info({'DOWN', _MonitorRef, _Type, {jc_sup, Node}, _Info}, 
	    #jc_psub_state{monitored = Ms} = S) ->

    lager:warning("~p: jc_sup down at ~p.", [?SERVER, Node]),
    signal_nodedown(Node),
    {noreply, S#jc_psub_state{monitored = lists:delete(Node, Ms)}};

% Subscribers that are monitored, produce a Down signal.
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
    lager:warning("~p: monitored client (~p) down.", [?SERVER, Object]),
    unsubscribe_all(Object),
    {noreply, State};

% Subscribers that are linked, produce an Exit signal.
handle_info({'EXIT', From, Reason}, State) ->
    lager:warning("~p: linked client (~p) down: ~p.", [?SERVER, From, Reason]),
    unsubscribe_all(From),
    {noreply, State};

% Time to remove any deadbeats -- clients (Pids) with no active subscriptions.
handle_info({evict_deadbeats, IntMs}, State) ->
    lager:debug("~p: evicting deadbeats.",[?SERVER]),
    remove_deadbeats(),
    schedule_deadbeat_eviction(IntMs),
    {noreply, State};

% mnesia write, record replaced: broadcast that change to subscribers.
handle_info({mnesia_table_event, {write, key_to_value, Rec, [Old], _Trx}}, State) ->

    notify_of_evict(Old),
    notify_of_write(Rec),

    {noreply, State};


% mnesia write, nothing replaced.
handle_info({mnesia_table_event, {write, key_to_value, Rec, [], _Trx}}, State) ->

    notify_of_write(Rec),

    {noreply, State};


handle_info({mnesia_table_event, {delete, key_to_value, _What, [Rec], _Trx}}, State) ->

    notify_of_delete(Rec),
    {noreply, State};


handle_info({mnesia_table_event, {delete, key_to_value, _What, [], _Trx}}, State) ->
    {noreply, State};


% flush delete's the table form the scheema
handle_info({mnesia_table_event, {delete, schema, {schema, _}, _What, _Trx}}, State) ->
    {noreply, State};

% flush adds the table to the sceema
handle_info({mnesia_table_event, {write, schema, {schema, _, _What}, _Rec, _Trx}}, State) ->

    {noreply, State};


handle_info(Info, State) ->
    lager:warning("~p: unexpected info message: ~p.",[?SERVER, Info]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private
%%
-spec terminate(any(), #jc_psub_state{}) -> any().

terminate(_Reason, _State) ->
    ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed
%%
-spec code_change(term(), #jc_psub_state{}, any()) -> {ok, #jc_psub_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%%  Internal functions
%%% ============================================================================


%% ----------------------------------------------------------------------------
%% @private Construct a nodeup topic event and send it.
%%
signal_nodeup(UpNode) ->
    {nodes, {active, A}, {configured, C}} = jc:cache_nodes(),
    topic_event(jc_node_events, {nodeup, UpNode, A, C}).


%% ----------------------------------------------------------------------------
%% @private Construct a nodedown topic event and send it.
%%
signal_nodedown(DownNode) ->
    {nodes, {active, A}, {configured, C}} = jc:cache_nodes(),
    % Mnesia might not know that the node is down yet, so remove
    % it explicitly from the list.
    topic_event(jc_node_events, {nodedown, 
				 DownNode, 
				 lists:delete(DownNode, A), 
				 C}).


%% ----------------------------------------------------------------------------
%% @private Notify subscribers of evicts, writes and deletes.
%%

notify_of_evict(#key_to_value{map = M, key = K}) ->
    lager:debug("~p: broadcast evict: {~p, ~p}.", [?SERVER, M, K]),
    broadcast_map_event({M, K, delete}, {M, K, delete}).


notify_of_write(#key_to_value{map = M, key = K, value = V}) ->
    lager:debug("~p: broadcast write: {~p, ~p}.", [?SERVER, M, K]),
    broadcast_map_event({M, K, write}, {M, K, write, V}).


notify_of_delete(#key_to_value{map = Map, key = Key}) ->
    lager:debug("~p: mnesia delete activity: {~p, ~p}.",
		[?SERVER, Map, Key]),
    broadcast_map_event({Map, Key, delete}, {Map, Key, delete}).


    
%% -----------------------------------------------------------------------------
%% @private Number of subscriptions + Pids listening as a proxy for load.
%% 
calculate_load()->
    F = fun() -> mnesia:foldl(fun(#ps_sub{clients = Clients}, Load) ->
				      Load + sets:size(Clients) + 1
			      end, 0, ps_sub)
	end,
    mnesia:async_dirty(F).
    

%% -----------------------------------------------------------------------------
%% @private Monitor a client (Pid), add its Pid to the client table.
%%
-spec monitor_client(pid()) -> ok.

monitor_client(Pid) ->
    lager:debug("~p: monitoring client: ~p:", [?SERVER, Pid]),
    
    F = fun() ->
		case mnesia:read(ps_client, Pid) of
		    [] ->
			Result = monitor_as_possible(Pid),
			mnesia:write(#ps_client{pid = Pid, m_type = Result});
		    [#ps_client{}] ->
			% process already being monitored
			ok
		end
	end,
    mnesia:transaction(F),
    ok.


%% -----------------------------------------------------------------------------
%% @private Demonitor a client and remove its Pid from the client table.
%%
-spec demonitor_client(Client::pid()) -> true | false.

demonitor_client(Pid) ->
    lager:debug("~p: demonitoring client: ~p.", [?SERVER, Pid]),

    F = fun() ->
		Result = case mnesia:read(ps_client, Pid) of
			     [#ps_client{m_type = {ref, Ref}}] ->
				 true = erlang:demonitor(Ref, [flush]);
			     [#ps_client{m_type = {link, _}}] -> 
				 true = erlang:unlink(Pid);
			     [] -> 
				 false
			 end,
		mnesia:delete({ps_client, Pid}),
		Result 
	end,
    mnesia:sync_dirty(F).


%% -----------------------------------------------------------------------------
%% Pids that cannot be monitored will be liked.
%%
monitor_as_possible(Pid) ->
    try erlang:monitor(process, Pid) of
	Ref -> {ref, Ref}
   catch
	error:_ ->
	    {link, erlang:link(Pid)}
    end.
    

%% -----------------------------------------------------------------------------
%% @private If the subscription does not exist, add it and set the client (Pid)
%% as its only subscriber. If the subscription exists, add the client (Pid)
%% to the set of subscribers. Monitor the client (Pid) so that if it goes away,
%% we can removie it as a subscriber.
%%
subscribe(Pid, Subscription ) ->
    lager:debug("~p: adding subscription: ~p for client: ~p.", 
		[?SERVER, Subscription, Pid]),

    monitor_client(Pid),

    F = fun() ->
		case mnesia:read(ps_sub, Subscription) of
		    []->
			mnesia:write(#ps_sub{subscription = Subscription, 
					     clients = sets:from_list([Pid])});
		    [#ps_sub{clients = Pids} = Rec]->
			PidSet = sets:add_element(Pid, Pids),
			mnesia:write(Rec#ps_sub{clients = PidSet})
		end
	end,
    mnesia:transaction(F).
		

%% -----------------------------------------------------------------------------
%% @private Assuming the subscription exists, remove the client (Pid) from the
%% set of listeners to that subscription.
%%
unsubscribe(Pid, Subscription) ->
    lager:debug("~p: deleting subscription: ~p for client: ~p.", 
		[?SERVER, Subscription, Pid]),
    F = fun() ->
		case mnesia:read(ps_sub, Subscription) of
		    [#ps_sub{clients = Pids} = Rec] ->
			NewSetOfPids = sets:del_element(Pid, Pids),
			case sets:size(NewSetOfPids) of
			    0 ->
				mnesia:delete_object(Rec);
			    _N ->
				mnesia:write(Rec#ps_sub{clients = NewSetOfPids})
			end;
		    []->
			ok
		end
	end,
    mnesia:transaction(F).


%% -----------------------------------------------------------------------------
%% @private Remove subscriber (Pid) from all subscriptions.
%%
unsubscribe_all(Pid) when is_pid(Pid) ->
    case demonitor_client(Pid) of
	true ->
	    F = fun(#ps_sub{clients = Pids} = Rec, _)->
			NewSetOfPids = sets:del_element(Pid, Pids),
			case sets:size(NewSetOfPids) of
			    0 ->
				mnesia:delete_object(Rec);
			    _N ->
				mnesia:write(Rec#ps_sub{clients = NewSetOfPids})
			end
		end,
	    Fld = fun() -> mnesia:foldl(F, [], ps_sub) end,
	    mnesia:transaction(Fld);
	false ->
	    ok
    end.


%% -----------------------------------------------------------------------------
%% Broadcast changes to subscribing pids. An Operation on a key could 
%% particiapte in 'any' subscriptions with respect to Keys or Operations. Ditto
%% for topics.
%%
broadcast_topic_event(Topic, Value)->
    lager:debug("~p: broadcasting topic event.", [?SERVER]),
    Variations = topic_variations(Topic, Value),
    [broadcast_change(Variation, {Topic, Value}) || Variation <- Variations].

topic_variations(Topic, Value)->
    [#topic_sub{topic = Topic, value = Value},
     #topic_sub{topic = Topic, value = any}].


broadcast_map_event(Event, Payload)->
    lager:debug("~p: broadcasting map event.", [?SERVER]),
    Variations = map_variations(Event),
    [broadcast_change(Variation, Payload) || Variation <- Variations].


map_variations({Map, Key, Operation}) ->
    [#map_sub{map = Map, key = Key, operation = Operation},
     #map_sub{map = Map, key = Key, operation = any},
     #map_sub{map = Map, key = any, operation = Operation},
     #map_sub{map = Map, key = any, operation = any}].


broadcast_change(Subscription, Message) ->
    F = fun() ->
		case mnesia:read(ps_sub, Subscription) of
		    [#ps_sub{clients = Set}] ->
			send_to_pids(Set, Message);
		    _else ->
			ok
		end
	end,
    mnesia:async_dirty(F).


%% -----------------------------------------------------------------------------
%% Send the Message to all of the Pids in the list Pids.
%%
send_to_pids(Pids, Message) ->
    sets:fold(fun(P, _) -> P ! Message, [] end, [], Pids).



%% -----------------------------------------------------------------------------
%% @private Create the message that gets sent back to jc_psub to trigger an 
%% eviction of any clients that have no active subscriptions.
%%
-spec schedule_deadbeat_eviction(non_neg_integer()) -> ok.

schedule_deadbeat_eviction(IntMs)->
    erlang:send_after(IntMs, ?SERVER, {evict_deadbeats, IntMs}),
    ok.


%% -----------------------------------------------------------------------------
%% Collect all Pids that are not in a subscription, demonitor and remove.
%%
-spec remove_deadbeats() -> ok.

remove_deadbeats() ->
    F = fun() ->
		PidsWithSubs = 
		    mnesia:foldl(fun(#ps_sub{clients=Set}, Acc) -> 
					 sets:union(Set, Acc) 
				 end,
				 sets:new(),
				 ps_sub),

		F2 = fun(#ps_client{pid=Pid}, Acc) ->
			     case sets:is_element(Pid, PidsWithSubs) of
				 true  -> Acc;
				 false -> [Pid|Acc]
			     end
		     end,
		DeadBeats = mnesia:foldl(F2, [], ps_client),
		[demonitor_client(Pid) || Pid<-DeadBeats]
	end,
    mnesia:transaction(F),
    ok.
