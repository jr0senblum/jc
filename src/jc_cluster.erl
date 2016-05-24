%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc Either Joins an existing Mnesia cluster, copying tables over to this
%%% replica; or create the first node of the cluster and creates relevant 
%%% tables. Created tables:
%%% <ul>
%%% <li>Table key_to_value stores the Map, Key, Value, Create Time Last Update
%%%     and a unique record reference. Time-stamps are unix-style, 
%%%     time-since-epoch, millisecond granularity.</li>
%%% <li>Table ttl maps the unique record reference to a timer_ref so that,
%%%     when the timer goes off, it can find the record that needs to be 
%%%     deleted - used by the jc_eviction_manager process. </li>
%%% <li>Table max_ttl holds {map_name, Secs} defining how long items in a
%%%     given map are allowed to live before being evicted. The eviction is
%%%     done via a process that runs at a configured interval. </li>
%%% <li>Table stats holds (currently) the datetime of application start.
%%%     and a ClusterId - the name of the Node that started the cluster -
%%%     that is used to recognize net joins after a split.</li>
%%% <li>Table ps_sub is a local table and uses a subscription pattern as a key 
%%%     that is associated to a set of process ids (PIDs) subscribed to the 
%%%     pattern.</li>
%%% <li>Table ps_client is a local table and has an entry for each subscribing 
%%%     PID and stores what mechansim is being used to monitor that node - link 
%%%     or monitor.</li>
%%% <li>Table seq stores the highest, per Map sequence number seen. Used by 
%%%     jc_s api that dissalows put and evict opperation with a sequence
%%%     number less than the greatest one seen -- helps with consistency. </li>
%%% <li>Tables to_index and auto_index support indexing in support of the _match
%%%     operations which search by json.path.criteria. auto_index tracks
%%%     the frequency of particular json.path.search.criteria and if that use
%%%     is sufficient, an index is initiated. The details of the index
%%%     are stored in the to_index table.</li>
%%% </ul>
%%%
%%% Desiged to create a diskless cluster (schema is RAM and all tables in RAM)
%%% therefore sys.config MUST CONTAIN {schema_location, ram}.
%%% @end
%%% Created : 17 Mar 2015 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(jc_cluster).
-compile(nowarn_deprecated_function). % accomidate now() for v < 18

%% API 
-export([init/0,
         change_cluster_id/0,
         check/1,
         get_cluster_id/0]).

 % Record and type definitions.
-include("../include/records.hrl").   

% Time to wait for mnesia tables to initialize when joining cluster.
-define(WAIT_FOR_TABLES_MS, 30000). 

-type cluster_id() :: node().


%%% ============================================================================
%%% Library Interface
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc If this node doesn't have the same ClusterId as the supplied one,
%% its an indciation of a net-join of a orphaned or island node. Likey to have
%% consistancy issues. Kill oneself and be restarted by the heartbeat.
%%
-spec check(ClusterId::cluster_id()) -> good | bad.

check(ClusterId) ->
    case get_cluster_id() of
        ClusterId ->
            lager:notice("~p: I (~p) have the correct cluster_id.", 
                         [?MODULE, node()]),
            good;
        no_exists -> 
            lager:notice("~p: I (~p) am joining an existing cluster, all good.",
                         [?MODULE, node()]),
            good;
        _WrongId ->
            lager:warning("~p: I (~p) have conflicting cluster_id, time to die.", 
                          [?MODULE, node()]),
            spawn(fun kamakazee/0),
            bad
    end.


%% -----------------------------------------------------------------------------
%% @doc Change the cluster_id: used when a Node that had been the ClusterId
%% Node of record dies.
%%
-spec change_cluster_id() -> ok.

change_cluster_id() ->
    mnesia:dirty_write(#stats{key = 'cluster_id', value = node()}),
    ok.
                        

%% -----------------------------------------------------------------------------
%% @doc Return the current cluster_id or no_exists if this Node hasn't made
%% it through mnesia cluster creation / join.
%% 
-spec get_cluster_id() -> cluster_id() | no_exists.

get_cluster_id() ->
    try mnesia:dirty_read(stats, 'cluster_id') of
        [#stats{value=Value}] ->
            Value
    catch
        exit:{aborted, {no_exists, _}} ->
            no_exists
    end.


% kill this node...
%
kamakazee() ->
    lager:notice("~p: ~p seppuku.", [?MODULE, node()]),
    exit(whereis(jc_sup), kill). 
    


%% -----------------------------------------------------------------------------
%% @doc Either join an existing Mnesia cluster or start one, creating the tables
%% as appropriate. Notice that sys.config MUST contain {schema_location, ram}.
%% 
-spec init() -> ok.

init()->
    Nodes = application:get_env(jc, cache_nodes, [node()]),
    [Node || Node <- Nodes, pong == net_adm:ping(Node) ],
    mnesia:stop(),
    global:sync(),
    global:trans({jc_mnesia, self()}, 
		 fun() ->
			 mnesia:start(),
			 case [Node || Node <- nodes(), jc_loaded(Node)] of
			     [] -> 
				 mnesia:create_schema([]),
				 dynamic_db_init([]),
				 Indexes = application:get_env(jc, indexes, []),
				 [jc_store:start_indexing(Map, Path) || 
				     {Map, Path} <- Indexes];
			     Ns ->
				 dynamic_db_init(Ns)
			 end,
			 true = global:del_lock({jc_mnesia, self()}),
			 ok
		 end,
		 [node() | nodes()], 
		 infinity).



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private Jcache is considered loaded if jc_psub is up - it is the last gen_server
%% started by the superviosr.
%%
-spec jc_loaded(Node::node()) -> true | false.

jc_loaded(Node) ->
    is_pid(rpc:call(Node, erlang, whereis, [jc_psub], 1000)).


%% -----------------------------------------------------------------------------
%% @private If there are existing nodes, add this one to the cluster and copy 
%% the needed tables to this replica. Otherwise, create the tables. 
%% 
-spec dynamic_db_init(CacheNodes::[node()] | []) -> ok.

dynamic_db_init([]) ->
    lager:info("Creating cluster"),

    mnesia:create_table(key_to_value,
			[{attributes, record_info(fields, key_to_value)},
			 {type, ordered_set},
			 {index, [map, key, ref, create_tm]}
			]),
    mnesia:create_table(seq,
			[{attributes, record_info(fields, seq)},
			 {type, set}
			]),
    mnesia:create_table(to_index,
			[{attributes, record_info(fields, to_index)},
			 {type, set},
			 {index, [map_name, position]}
			]),
    mnesia:create_table(auto_index,
			[{attributes, record_info(fields, auto_index)},
			 {type, set}
			]),
    mnesia:create_table(ttl,
			[{attributes, record_info(fields, ttl)}
			]),
    mnesia:create_table(max_ttl,
			[{attributes, record_info(fields, max_ttl)},
			 {type, set}
			]),
    mnesia:create_table(stats,
			[{attributes, record_info(fields, stats)}
			]),
    mnesia:create_table(ps_sub,
			[{attributes, record_info(fields, ps_sub)},
			 {local_content, true}
			]),
    mnesia:create_table(ps_client,
			[{attributes, record_info(fields, ps_client)},
			 {local_content, true}
			]),

    mnesia:dirty_write(#stats{key = 'jc_store_up_time', 
                              value = calendar:now_to_datetime(timestamp())}),

    mnesia:dirty_write(#stats{key = 'cluster_id', value = node()}),

    ok;

dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).


add_extra_nodes([Node|Nds]) ->
    Wait = application:get_env(jc, table_wait_ms, ?WAIT_FOR_TABLES_MS),
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok, [Node]} ->
	    lager:info("~p: joining cluster: ~p", [?MODULE, nodes()]),
	    mnesia:add_table_copy(schema, node(), ram_copies),
	    mnesia:add_table_copy(stats, node(), ram_copies),
	    mnesia:add_table_copy(key_to_value, node(), ram_copies),
	    mnesia:add_table_copy(seq, node(), ram_copies),
	    mnesia:add_table_copy(auto_index, node(), ram_copies),
	    mnesia:add_table_copy(to_index, node(), ram_copies),
	    mnesia:add_table_copy(ttl, node(), ram_copies),
	    mnesia:add_table_copy(max_ttl, node(), ram_copies),
	    mnesia:add_table_copy(ps_sub, node(), ram_copies),
	    mnesia:add_table_copy(ps_client, node(), ram_copies),
	    Tables = mnesia:system_info(tables),
	    ok = mnesia:wait_for_tables(Tables, Wait),
	    ok;
	_ ->
	    add_extra_nodes(Nds)
    end.


% Try to used erlang 18+ timestamp(), support older versions if necessary.
timestamp() ->
    try
	erlang:timestamp()
    catch
	error:undef ->
	    erlang:now()
end.
