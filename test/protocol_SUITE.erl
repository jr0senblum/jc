%% 


-module(protocol_SUITE).

-include("../include/records.hrl").
-inclde_lib("common_test/include/ct.hrl").


-export([all/0, 
	 init_per_suite/1,
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 end_per_suite/1]).

-export([put_get_test/1, error_test/1]).


% So we can test that all erlang types are valid keys, values and maps
-record(rec, {value}).


all() ->
    [ put_get_test, error_test
].

init_per_suite(Config) ->
    net_kernel:start(['jc1@127.0.0.1', longnames]),
    application:set_env(jc, cache_nodes, ['jc1@127.0.0.1','jc2@127.0.0.1', 'jc3@127.0.0.1']),
    application:set_env(jc, max_ttl_maps, [{testmap, 100}]),
    application:set_env(jc,  indexes, [{bed, "identifier"},
					{bed, "menu.2.id.'2'"},
					{cow, "cow.2.id.'2'"}]),
    application:set_env(jc,  analyze_freq, {5, 5}),
    application:set_env(jc,  protocol_port, 5555),

    application:ensure_all_started(jc),
    lager:set_loglevel(lager_console_backend, debug),
    Config.


init_per_testcase(_, Config) ->
    code:load_file(t),
    t:start_link(),
    <<"{\"version\":1.0}">> = get_result(),

    {maps, Maps} = jc:maps(),
    [jc:clear(Map) || Map <- Maps],
    Config.

end_per_testcase(_, Config) ->
    jc:flush(silent),
    t:terminate(),
    Config.

end_per_suite(Config) ->
    jc:stop(),
    Config.


error_test(_Config) ->
    t:send("{put, bed, 1, 1"),
    <<"{\"error\":\"command_syntax\"}">> = get_result(),
    
    t:send("{what, bed, 1, 1}"),
   <<"{\"error\":\"unrecognized_command\"}">> = get_result().



put_get_test(_Config) ->

    t:send("{put, bed, 1, 1}"),
    <<"{\"ok\":1}">> = get_result(),
    
    t:send("{put, bed, 3, 3.3}"),
    <<"{\"ok\":3}">>= get_result(),
    
    t:send("{put, bed, 4, [1,2,3]}"),
    <<"{\"ok\":4}">> = get_result(),

    J1 = "\"{\\\"first\\\":{\\\"second\\\":1, \\\"third\\\":true},\\\"second\\\":true}\"",
    J2 = "\"{\\\"first\\\":{\\\"second\\\":2, \\\"third\\\":false},\\\"second\\\":true}\"",

    t:send("{put, json, 1, " ++ J1 ++ "}"),
   <<"{\"ok\":1}">> = get_result(),

    t:send("{put, json, 2, " ++ J2 ++ "}"),
    <<"{\"ok\":2}">> = get_result(),

    t:send("{get, bed, 1}"),
    <<"{\"ok\":1}">> = get_result(),

    t:send("{get, bed, 3}"),
    <<"{\"ok\":3.3}">> = get_result(),

    t:send("{get, bed, 4}"),
    <<"{\"ok\":[1,2,3]}">> = get_result(),

    
    t:send("{put_all, evs, [{1,1}, {2,\"2\"}, {3,3.3}]}"),
    <<"{\"ok\":3}">> = get_result(),   

    t:send("{get_all, evs, [1, 2, 4]}"),
    <<"{\"hits\":{\"2\":\"2\",\"1\":1},\"misses\":[4]}">>  = get_result(),
    
    t:send("{key_set, bed}"),
    <<"{\"ok\":[1,3,4]}">> = get_result(),

    t:send("{values, bed}"),
    <<"{\"ok\":[1,3.3,[1,2,3]]}">> = get_result(),

    t:send("{values_match, json, \"first.second=1\"}"),
    R = list_to_binary("{\"ok\":{\"1\":" ++ J1 ++ "}}"),
    R = get_result(),
    
    t:send("{values_match, json, \"second=true\"}"),

    R2 = list_to_binary("{\"ok\":{\"1\":" ++ J1 ++ ",\"2\":" ++ J2 ++ "}}"),
    R2 = get_result().


    

get_result() ->
    receive
	X -> X
    after 
	400 ->
	    error
    end.

    
