


-module(protocol_SUITE).

-include("../include/records.hrl").
-inclde_lib("common_test/include/ct.hrl").


-export([all/0, 
	 init_per_suite/1,
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 end_per_suite/1]).

-export([get_maps_test/1]).


all() -> [ get_maps_test].


init_per_suite(Config) ->
    net_kernel:start(['jcREST@127.0.0.1', longnames]),
    application:set_env(jc, cache_nodes, ['jc1@127.0.0.1','jc2@127.0.0.1', 'jc3@127.0.0.1']),
    application:set_env(jc, rest_server, true),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jc),
    lager:set_loglevel(lager_console_backend, debug),
    jc:flush(silent),
    _ = inets:start(),
    Config.


init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    jc:flush(silent),
    Config.

end_per_suite(Config) ->
%    jc:stop(),
%    inets:stop(),
    Config.


get_maps_test(_Config) ->
    _ = [jc:put(<<"map1">>,list_to_binary(integer_to_list(I)),1) || I <- lists:seq(1,5)],
    _ = [jc:put(<<"map2">>,list_to_binary(integer_to_list(I)),1) || I <- lists:seq(1,5)],
    _ = [jc:put(<<"map3">>,list_to_binary(integer_to_list(I)),1) || I <- lists:seq(1,5)],
    {ok,{{"HTTP/1.1",200,"OK"},
     [_date, _server, _content_length, {"content-type","application/json"}],
         "{\"maps\":[{\"map\":\"map3\",\"links\": [{\"rel\":\"self\",\"href\":\"http://127.0.0.1:8080/maps/map3\"}]},{\"map\":\"map2\",\"links\": [{\"rel\":\"self\",\"href\":\"http://127.0.0.1:8080/maps/map2\"}]},{\"map\":\"map1\",\"links\": [{\"rel\":\"self\",\"href\":\"http://127.0.0.1:8080/maps/map1\"}]}], \"links\": [{\"rel\":\"self\",\"href\":\"http://127.0.0.1:8080/maps\"}]}"}} = httpc:request(get, {"http://127.0.0.1:8080/maps", []}, [], []).
    
