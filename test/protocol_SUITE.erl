


-module(protocol_SUITE).

-include("../include/records.hrl").
-inclde_lib("common_test/include/ct.hrl").


-export([all/0, 
	 init_per_suite/1,
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 end_per_suite/1]).

-export([put_get_test/1, error_test/1, quant_test/1]).


all() ->
    [ put_get_test, error_test, quant_test
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
    lager:set_loglevel(lager_console_backend, info),
    Config.


init_per_testcase(_, Config) ->
    code:load_file(t),
    t:start_link(),
    <<"{\"version\":\"1.0\"}">> = get_result(),

    {maps, Maps} = jc:maps(),
    [jc:clear(Map) || Map <- Maps],
    Config.

end_per_testcase(_, Config) ->
    jc:flush(silent),
    t:send("{close}"),
    get_all_results(),
    Config.

end_per_suite(Config) ->
    jc:stop(),
    Config.


error_test(_Config) ->
    t:send("{put, bed, \"1\", 1"),
    <<"{\"error\":\"command_syntax\"}">> = get_result(),
    
    t:send("{what, bed, \"1\", 1}"),
   <<"{\"error\":\"unrecognized_command\"}">> = get_result().



put_get_test(_Config) ->

    t:send("{put, bed, \"1\", 1}"),
    <<"{\"ok\":\"1\"}">> = get_result(),
    
    t:send("{put, bed, \"3\", 3.3}"),
    <<"{\"ok\":\"3\"}">>= get_result(),
    
    t:send("{put, bed, \"4\", [1,2,3]}"),
    <<"{\"ok\":\"4\"}">> = get_result(),

    J1 = "\"{\\\"first\\\":{\\\"second\\\":1, \\\"third\\\":true},\\\"second\\\":true}\"",
    J2 = "\"{\\\"first\\\":{\\\"second\\\":2, \\\"third\\\":false},\\\"second\\\":true}\"",

    t:send("{put, json, \"1\", " ++ J1 ++ "}"),
   <<"{\"ok\":\"1\"}">> = get_result(),

    t:send("{put, json, \"2\", " ++ J2 ++ "}"),
    <<"{\"ok\":\"2\"}">> = get_result(),

    t:send("{get, bed, \"1\"}"),
    <<"{\"ok\":1}">> = get_result(),

    t:send("{get, bed, \"3\"}"),
    <<"{\"ok\":3.3}">> = get_result(),

    t:send("{get, bed, \"4\"}"),
    <<"{\"ok\":[1,2,3]}">> = get_result(),

    
    t:send("{put_all, evs, [{1,1}, {2,\"2\"}, {\"3\",3.3}]}"),
    <<"{\"ok\":3}">> = get_result(),   

    t:send("{get_all, evs, [1, 2, 4]}"),
    <<"{\"hits\":[{\"key\":2,\"value\":\"2\"},{\"key\":1,\"value\":1}],\"misses\":[4]}">>  = get_result(),
    
    t:send("{key_set, bed}"),
    <<"{\"ok\":[\"1\",\"3\",\"4\"]}">> = get_result(),

    t:send("{values, bed}"),
    <<"{\"ok\":[1,3.3,[1,2,3]]}">> = get_result(),

    t:send("{values_match, json, \"first.second=1\"}"),
    R = list_to_binary("[{\"key\":\"1\",\"value\":" ++ J1 ++ "}]"),
    R = get_result(),
    
    t:send("{values_match, json, \"second=true\"}"),

    R2 = list_to_binary("[{\"key\":\"1\",\"value\":" ++ J1 ++ "},{\"key\":\"2\",\"value\":" ++ J2 ++ "}]"),
    R2 = get_result(),

    t:send("{put, bed, \"1\", 1}"),
    <<"{\"ok\":\"1\"}">> = get_result(),
    
    t:send("{evict, evs, 1}"),
    <<"\"ok\"">> = get_result(),

    t:send("{map_size, evs}"),
    <<"{\"records\":2}">> = get_result(),

    t:send("{map_size, json}"),
    <<"{\"records\":2}">> = get_result(),

    t:send("{evict_all_match, \"first.second=1\"}"),
    <<"\"ok\"">> = get_result(),

    t:send("{map_size, json}"),
    <<"{\"records\":1}">> = get_result(),
    
    t:send("{contains_key, evs, 2}"),
    <<"true">> = get_result(),

    t:send("{contains_key, evs, 123}"),
    <<"false">> = get_result(),

    t:send("{cache_nodes}"),
    <<"{\"active\":[\"jc1@127.0.0.1\"],\"configured\":[\"jc1@127.0.0.1\",\"jc2@127.0.0.1\",\"jc3@127.0.0.1\"]}">>
        = get_result(),
    
    t:send("{flush}"),
    <<"\"ok\"">> = get_result(),

    t:send("{cache_size}"),
    <<"{\"key_to_value\":", _/binary >> = get_result(),

    t:send("{map_size, bed}"),
    <<"{\"records\":0}">> = get_result(),

    t:send("{put_s, bed, \"1\", 1, 20}"),
    <<"{\"ok\":\"1\"}">> = get_result(),

    t:send("{put_s, evs, \"1\", 1, 30}"),
    <<"{\"ok\":\"1\"}">> = get_result(),

    t:send("{maps}"),
    <<"{\"maps\":[\"bed\",\"evs\"]}">> = get_result(),
    
    t:send("{up}"),
    <<"{\"uptime\":", _/binary>> = get_result(),

    t:send("{sequence}"),
    <<"{\"bed\":20,\"evs\":30}">> = get_result(),

    t:send("{sequence, map}"),
    <<"0">> = get_result(),

    t:send("{set_max_ttl, bed, 1000}"),
    <<"\"ok\"">> = get_result(),

    t:send("{get_max_ttls}"),
    <<"{\"bed\":1000,\"testmap\":100}">> = get_result().



quant_test(_config) ->
    Limit  = 5000,
    [ t:send("{put, bed, \"" ++ integer_to_list(X) ++ "\", " ++ integer_to_list(X) ++ "}") ||
	X <- lists:seq(1, Limit)],
    R = get_all_results(),
    Limit = length(R) - 1,
    t:send("{map_size, bed}"),
    <<"{\"records\":5000}">> = get_result().


get_all_results() ->
    receive X ->
	    [X|get_all_results()]
    after
	1000 ->
	    [ok]
    end.

get_result() ->
    receive
	X -> X
    after 
	400 ->
	    error
    end.

    
