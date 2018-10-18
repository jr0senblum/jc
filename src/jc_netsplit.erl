%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MacBook-Pro.local>
%%% @copyright (C) 2016, Jim Rosenblum 
%%% @doc Mnesia does not merge on its own after a net split. Also, there
%%% is a chance that two Nodes start-up unaware of one another and then the
%%% erlang mesh is created; or a race condition allows two Nodes to create
%%% independent mnesia 'nodes'. So, each cluster instance will get a unique
%%% ClusterId - the Node name of the Node that created the cluster. 
%%%
%%% If a Node with a cluster's ClusterID dissapears, a new ClusterID will 
%%% be established from one of the surviving Nodes.
%%%
%%% When a Node appears it will either have the same ClusterId
%%% as everyone else (all is good), or not (bad). If bad, we should kill all 
%%% Nodes such that there is one, consistant ClusterId and then flush.
%%%
%%% cluster creation: ClusterId established. 
%%%
%%% nodedown: ClusterId is changed if the Node whose name = ClusterId went 
%%% down. When there is a split one island is guaranteed to change its 
%%% ClusterId
%%%
%%% nodeup: Either 
%%% A Node appears having joined the existing cluster and therefore will report 
%%% the same ClusterId - good; or 
%%%
%%% It appears believing it is part of a different cluster in which case the 
%%% node will report different ClusterId - bad. 
%%% 
%%% For any bad outcome, all nodes having the 'different' ClusterId are killed
%%% to be restarted by the heart process, and a survivor may do a flush per
%%% policy in configy.sys
%%%
%%% @end
%%% Created : 18 May 2016 by Jim Rosenblum <jrosenblum@Jims-MacBook-Pro.local>
%%% ----------------------------------------------------------------------------
-module(jc_netsplit).

-behaviour(gen_server).


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).


-define(SERVER, ?MODULE).

-define(LOCK, {?MODULE, self()}).


% State: list of configured nodes, and flush policy on join after net split.
-record(jc_ns_state, 
        {nodes = [] :: [Configured::node()],
         should_flush = true :: boolean()}).



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
%% @private Initialize the server by subscribing to node events from net_kernel
%% and grabbing the list of configured Nodes.
%%
-spec init([]) -> {ok, #jc_ns_state{}}.

init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, Configured} = application:get_env(jc, cache_nodes),
    {ok, ShouldFlush} = application:get_env(jc, should_flush),
    
    lager:info("~p: up and watching events for ~p.", [?SERVER, Configured]),

    {ok, #jc_ns_state{nodes = lists:sort(Configured), 
                      should_flush = ShouldFlush}}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages: None.
%%
-spec handle_call(term(), {pid(), _}, #{}) -> {reply, ok, #{}}.

handle_call(Request,  _From, State) ->
    lager:warning("~p: unexpected call request: ~p.",[?SERVER, Request]),
    {reply, ok, State}.


%% -----------------------------------------------------------------------------
%% @private Handle cast messages: None.
%%
-spec handle_cast(any(), #{}) -> {noreply, {}}.

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.",[?SERVER, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Handle info message: dis/apperations.
%%
-spec handle_info(any(), #{}) -> {noreply, #{}}.

handle_info({nodeup, Upped}, #jc_ns_state{nodes = Ns, should_flush = Sf} = State) ->
    check_cluster_health(Upped, Ns, Sf),
    {noreply, State};
    
handle_info({nodedown, Downed}, State) ->         
    change_cluster_id(Downed),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("~p: unexpected info message: ~p.",[?SERVER, Info]),
    {noreply, State}.


%% @private Terminate server.
%%
-spec terminate(any(), #{}) -> any().
terminate(_Reason, _State) ->
    ok.


%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #{}, any()) -> {ok, #{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal Functions
%%% ============================================================================


% When node disappears, change the cluster_id. Only one node will do this.
%
change_cluster_id(Downed) ->
    case jc_cluster:get_cluster_id() of
        Downed ->
            do_change(Downed);
        _ ->
            lager:notice("~p: ~p down, but not a cluster owner, no action.",
                         [?MODULE, Downed]),
            ok
    end.

do_change(Downed) ->
    global:trans(?LOCK,
                 fun() ->
                         case jc_cluster:get_cluster_id() of
                             Downed ->
                                 New = node(),
                                 jc_cluster:change_cluster_id(),
                                 lager:notice("~p: changing id from ~p to ~p.", 
                                              [?SERVER, Downed, New]),
                                 New;
                             Other ->
                                 lager:notice("~p: id has already changed.",
                                              [?MODULE]),
                                 Other
                         end
                 end,
		 [node() | nodes()], 
		 infinity).



% Each mnesia cluster has an associated ClusterId. If we have one cluster than
% all nodes will report the same ClusterId. If not, then 'outliers' should 
% kill themselves and let the hearbeat process restart them.
%
check_cluster_health(Upped, Nodes, ShouldFlush) ->
    case is_relevant(Upped, Nodes) of
        false ->
            ok;
        true ->
            check(Upped, Nodes, jc_cluster:get_cluster_id(), ShouldFlush),
            ok
    end.




% Ask each node to check that it has the same ClusterId. Any  node that has
% a different ClusterId will kill itself and be restarted by the heartbeat
% process. If any nodes were killed, flush the entire cache per policy in 
% sys.config
%
check(Upped, Nodes, ClusterId, ShouldFlush) ->
    {Res, _Bad} = 
        global:trans(?LOCK,
                     fun() -> 
                             lager:notice("~p: ~p appeared, checking integrety.", 
                                          [?SERVER, Upped]),
                             rpc:multicall(lists:delete(node(), Nodes), 
                                           jc_cluster, 
                                           check, 
                                           [ClusterId], 
                                           500)
                     end,
                     [node() | nodes()], 
                     infinity),

    case lists:member(bad, Res) of
        true when ShouldFlush ->
            lager:notice("~p: cluster repaired, flush per policy.", 
                         [?SERVER]),
            jc:flush();
        true when not ShouldFlush ->
            lager:notice("~p: cluster repaired, not flushing per policy.", 
                         [?SERVER]);
        false ->
            lager:notice("~p: cluster showed no signs of inconsistency.", 
                         [?SERVER])
    end.
    


% If the node that dis/apperated is one of the configured cache nodes, 
% then it is relevant.
%
is_relevant(Subject, Nodes) ->
    lists:member(Subject, Nodes).
