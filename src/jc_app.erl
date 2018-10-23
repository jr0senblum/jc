%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2015, Jim Rosenblum
%%% @doc Application module for the JC application.
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(jc_app).

-behaviour(application).


%% Application callbacks.
-export([start/2, stop/1]).


%%% ============================================================================
%%% Application callbacks
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private Ask jc_store to initialize or join the the mnesia cluster, start
%% the tcp listeners (for protocol users), and fire-up the top level supervisor.
%% 
-spec start (normal | {takeover   | failover, atom()}, [{node, atom()}]) -> 
   		      {ok, pid()} | {error, atom()}.

start(_StartType, _StartArgs) ->
    ok = jc_cluster:init(),
    Port = application:get_env(jc, protocol_port, 5555),

    Dispatch = cowboy_router:compile([
		{'_', [
                       {"/maps", cb_collections_h, [maps]},
                       {"/maps/:map", cb_collections_h, [map]}
		]}
	]),
    
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], 
                                     #{env => #{dispatch => Dispatch}
	}),


    lager:info("tcp, protocol listener is up and listening on port: ~p", [Port]), 
    case jc_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.


%% -----------------------------------------------------------------------------
%% @doc Called when the application is stopped.
%% 
-spec stop(term()) -> ok.

stop(_State) ->
    ok.
