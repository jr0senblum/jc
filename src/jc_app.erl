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
    maybe_start_REST_service(application:get_env(jc, rest_server, false)),
 
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


maybe_start_REST_service(false) ->
    ok;
maybe_start_REST_service(true) ->
    RestIP = application:get_env(jc, rest_server_ip, "127.0.0.1"),
    RestPort = application:get_env(jc, rest_server_port, 8080),
    RestRoot = application:get_env(jc, rest_server_root, "/"),

    Dispatch = cowboy_router:compile([
                                      {RestIP, [
                                                {RestRoot ++ "/maps/:map/search/:path", cb_collections_h, [search]},
                                                {RestRoot ++ "/maps/:map/:key", cb_map_h, [key]},
                                                {RestRoot ++ "/maps/:map", cb_collections_h, [map]},
                                                {RestRoot ++ "maps", cb_collections_h, [maps]}
                                            ]}
                                     ]),

    {ok, _} = cowboy:start_clear(http, [{port, RestPort}], 
                                 #{env => #{dispatch => Dispatch}
                                  }),

    lager:info("~p: RESTFUL http server is up and listening on host: ~p, port: ~p, path: ~p.",
               [?MODULE, RestIP, RestPort, RestRoot]).
