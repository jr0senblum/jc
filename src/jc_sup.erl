%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2015, Jim Rosenblum
%%% @doc Top-level supervisor for the jc application. 
%%% 
%%% @version {@version}
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(jc_sup).

-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%% ============================================================================
%%% API functions
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc Start the supervisor.
%%
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%% ============================================================================
%%% Supervisor callbacks
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private Fire up the supervised processes.
%%
-spec init([]) ->  {'ok', {{'one_for_one', 60, 3600}, 
                           [supervisor:child_spec()]}}.

init([]) ->
    EM = {jc_eviction_manager, 
	  {jc_eviction_manager, start_link, []},
	  permanent, 2000, worker, [jc_eviction_manager]},

    PS = {jc_psub, 
	  {jc_psub, start_link, []},
	  permanent, 2000, worker, [jc_psub]},

    JS = {jc_sequence, 
	  {jc_sequence, start_link, []},
	  permanent, 2000, worker, [jc_sequence]},

    JA = {jc_analyzer, 
	  {jc_analyzer, start_link, []},
	  permanent, 2000, worker, [jc_analyzer]},

    JB = {jc_bridge, 
	  {jc_bridge, start_link, []},
	  permanent, 2000, worker, [jc_bridge]},

    {ok, {{one_for_one, 60, 3600}, [PS, EM, JS, JA, JB]}}.
