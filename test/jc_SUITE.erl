%% compile first, then invoke common test with 


-module(jc_SUITE).
-compile(nowarn_deprecated_function). % accomidate now() for v < 18
-compile(nowarn_warn_missing_spec). 

-include("../include/records.hrl").
-inclde_lib("common_test/include/ct.hrl").


-export([all/0, 
	 init_per_suite/1,
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 end_per_suite/1]).

-export([seq_test/1]).
-export([sysconfig_test/1]).
-export([meta_data_test/1]).

-export([maps_test/1, map_size_test/1]).

-export([put_simple_test/1,
	 put_seq_test/1,
	 put_eviction_test/1,
	 put_all_test/1]).

-export([evict_simple_test/1, evict_match_test/1, evict_match_s_test/1, values_match_test/1]).

-export([remove_items_test/1,
	 clear_test/1,
	 max_ttl_test/1, max_ttl_evict_test/1,
	 delete_map_since_test/1]).

-export([indexing_test/1, indexed_match_test/1]).

-export([map_subscribe_test/1, topic_subscribe_test/1]).

-export([cluster_test/1]).
-export([auto_analyzer_test/1]).



% So we can test that all erlang types are valid keys, values and maps
-record(rec, {value}).
-define(ALL_TYPES, [-2, 0, 2, 2.2, two, <<2>>, <<"2">>, {a, tuple}, #{value=>2},
		    [2,2], "2", #rec{value=2}, true, make_ref(), 
		    fun()->1 end, self()]).


all() ->
    [ sysconfig_test,auto_analyzer_test,
     meta_data_test, 
     maps_test, map_size_test,
     put_simple_test, put_seq_test, put_eviction_test, put_all_test,
     evict_simple_test, evict_match_test, evict_match_s_test, values_match_test,
     remove_items_test, clear_test, 
     max_ttl_test, max_ttl_evict_test,
     indexing_test, indexed_match_test,
     delete_map_since_test,
     map_subscribe_test, topic_subscribe_test,
     cluster_test,seq_test

].

init_per_suite(Config) ->
    net_kernel:start(['jc1@127.0.0.1', longnames]),
    application:set_env(jc, cache_nodes, ['jc1@127.0.0.1','jc2@127.0.0.1', 'jc3@127.0.0.1']),
    application:set_env(jc, max_ttl_maps, [{testmap, 100}]),
    application:set_env(jc,  indexes, [{bed, "identifier"},
					{bed, "menu.2.id.'2'"},
					{cow, "cow.2.id.'2'"}]),
    application:set_env(jc,  analyze_freq, {5, 5}),

    application:ensure_all_started(jc),
    lager:set_loglevel(lager_console_backend, error),
    [{maps, [bed, evsRequest]} | Config].


init_per_testcase(_, Config) ->
    {maps, Maps} = bridge({maps}),
    [bridge({clear, Map}) || Map <- Maps],
    Config.

end_per_testcase(_, Config) ->
    jc:flush(),
    Config.

end_per_suite(Config) ->
    jc:stop(),
    Config.



auto_analyzer_test(_Confi) ->
    [] = jc_analyzer:dump(),
    Details = jc_store:indexes(),
    [mnesia:transaction(fun() -> mnesia:delete({to_index, {Map, BPath}}) end) || 
	{{Map, BPath},_} <- Details],
    jc:evict_match(bed, "test=1"),
    jc:values_match(bed, "test=1"),
    jc:evict_match(bed, "test=1"),
    jc:values_match(bed, "test=1"),

    [{auto_index,{bed,{<<"test">>}},
             4,F,L,
             [F, _,_ ,L],
             false,undefined, none}] = jc_analyzer:dump(),
    timer:sleep(6000),

    jc:values_match(bed, "test=1"),
    [{auto_index,{bed,{<<"test">>}},
             1, _First, Last,
             [Last],
             false,undefined,none}] = jc_analyzer:dump(),
    
    [] = jc_store:indexes(),
    jc:evict_match(bed, "test=1"),
    jc:evict_match(bed, "test=1"),
    jc:evict_match(bed, "test=1"),
    jc:evict_match(bed, "test=1"),
    jc:evict_match(bed, "test=1"),
    timer:sleep(500),
 
    [{auto_index,{bed,{<<"test">>}},
      6, _, _, _, true, true, none}] = jc_analyzer:dump(),
    
    [{{bed,{<<"test">>}},6}] = jc_store:indexes(),
    jc_store:stop_indexing(bed, "test").
    

    
% sequence numbers are map specific
% are created when a map if first seen
% are deleted on a jc:flush
% are not removed by evicting individual 
seq_test(_Config) ->
    [] = bridge({sequence}),
    bridge({put, a,1,1}),
    bridge({put_s, f,1,1,1}),
    bridge({put_s, e,1,1,1}),
    bridge({put_s, d,1,11,11}),
    bridge({put_s, z,1,10,10}),
    bridge({put_s, z,1,10,1}),
    bridge({put_s, d,1,11,10}),
    bridge({put_s, aa,1,1,1}),
    [{aa,1},{d,11},{e,1},{f,1},{z,10}] = bridge({sequence}),

    11 = bridge({sequence, d}),
    1 = bridge({sequence, f}),
    0 = bridge({sequence, wrong}),
    jc:flush(),
    [] = bridge({sequence}).


% max_ttls should be pulled from sys.config
% setting a ttl to 0 should remove it, negative # or non-number is badarg
% indexes should be pulled from sys.config
% when you stop indexing they should disappear
sysconfig_test(_Confi) ->
    [{testmap, 100}] = jc_eviction_manager:get_max_ttls(),
    ok = jc_eviction_manager:set_max_ttl(testmap, 50),
    [{testmap, 50}] = jc_eviction_manager:get_max_ttls(),
    ok = jc_eviction_manager:set_max_ttl(testmap, 0),
    [] = jc_eviction_manager:get_max_ttls(),
    {error, badarg} = jc_eviction_manager:set_max_ttl(testmap, -100),
    {error, badarg} = jc_eviction_manager:set_max_ttl(testmap, c),

    [{{bed,{<<"identifier">>}}, 6},
     {{bed,{<<"menu">>,2,<<"id">>, <<"2">>}},7},
     {{cow,{<<"cow">>,2,<<"id">>, <<"2">>}},6}] = lists:sort(jc_store:indexes()),
    

    [{{bed,{<<"identifier">>}}, 6},
     {{bed,{<<"menu">>,2,<<"id">>, <<"2">>}},7}] = lists:sort(jc_store:indexes(bed)),

    ok = jc_store:stop_indexing(bed, "identifier"),
    ok = jc_store:stop_indexing(bed, "menu.2.id.'2'"),
    ok = jc_store:stop_indexing(cow, "cow.2.id.'2'"),
    [] = jc_store:indexes().


    
% meta data tests: up() should return up-time tuple with reasonable sec calc.
% cachce_size should return 8 tables all empty except for schema and stats
% cache_nodes should return all active and configured nodes

meta_data_test(_Config) ->
    {uptime, [{up_at, Up},
	      {now, NowIs},
	      {up_time, {0, {0, 0, Secs}}}]} = bridge({up}),

    Now =  calendar:now_to_datetime( timestamp()),
    NowString = string:left(httpd_util:rfc1123_date(Now), 16),
    NowString = string:left(Up, 16),
    NowString = string:left(NowIs, 16),
    true = (Secs < 60),

    {size, T} = bridge({cache_size}),
    
    [{auto_index,{records,0},{bytes,_}},
     {key_to_value,{records,0},{bytes,_}},
     {max_ttl,{records,0},{bytes,_}},
     {ps_client,{records,1},{bytes,_}},
     {ps_sub,{records,1},{bytes,_}},
     {schema,{records,10},{bytes,_}},
     {seq,{records,0},{bytes,_}},
     {stats,{records,2},{bytes,_}},
     {to_index,{records,0},{bytes,_}},
     {ttl,{records,0},{bytes,_}}] = lists:sort(T),
    
    {error, badarg} = jc_store:stats(wrong),


    {{active, [Active]}, {configured, C}} = bridge({cache_nodes}),
    Configured = application:get_env(jc, cache_nodes, []),
    true = (Active == node()),
    true = (lists:sort(Configured) == lists:sort(C)).


% maps should return all maps in use
% putting should create map if not there
% put_s should create map if not there
% deleting last item from map should remove it from use
maps_test(_Config) ->

    false = jc:map_exists(bed),
    {maps, []} = bridge({maps}),
    {ok, 1} = bridge({put, bed, 1, 1}),
    {ok, 1} = bridge({put, evs, 1, 1}),
    {ok, 2} = bridge({put, evs, 2, 2}),
    {ok, 2} = bridge({put_s, trx, 2, 2, 22}),
    {maps, [bed, evs, trx]} = bridge({maps}),
    true = jc:map_exists(bed),
    bridge({evict, bed, 1}),
    {maps, [evs, trx]} = bridge({maps}),
    bridge({evict, evs, 1}),
    bridge({evict, evs, 2}),
    {maps, [trx]} = bridge({maps}),
    bridge({evict_s, trx, 1, 1}),
    {maps, [trx]} = bridge({maps}),
    bridge({evict_s, trx, 2, 33}),
    {maps, []} = bridge({maps}).

% map_size returns number of elements in map
% putting incr number of elements
% evicting decr number of elements
% put_s incr number of elements when seq is good
% evicting_s decr number of elements when seq is good
map_size_test(_Config) ->
    {records, 0} = bridge({map_size, bed}),

    _ = [jc:put(bed, V, V)|| V <- lists:seq(1, 100)],
    {records, 100} = bridge({map_size, bed}),

    _ = [jc:evict(bed, V)|| V <- lists:seq(1, 100,2)],
    {records, 50} = bridge({map_size, bed}),
    jc:flush(),
    
    {records, 0} = bridge({map_size, bed}),

    _ = [bridge({put_s, bed, V, V, V})|| V <- lists:seq(1, 100)],
    {records, 100} = bridge({map_size, bed}),

    _ = [bridge({evict_s, bed, V, V+100})|| V <- lists:seq(1, 100,2)],
    {records, 50} = bridge({map_size, bed}),

    _ = [bridge({evict_s, bed, V, V})|| V <- lists:seq(1, 100,2)],
    {records, 50} = bridge({map_size, bed}).



% Get what you put
% All erlang types can be keys, values and maps for put and put_s.
% Ttl removes records as appropriate
put_simple_test(_Config) ->
    Map = bed,
    _ =[begin 
	    V = K * 2 ,
	    {ok,K} = bridge({put, Map, K, V}),
	    {ok, V} = bridge({get, Map, K})
	end || K <- lists:seq(1,100)],
    
    _ = [begin
	     {ok, W} = bridge({put, W, W, W, 1}),
	     {ok, W} = bridge({get, W, W}),
	     {records, 1} = jc:map_size(W)
	 end || W <- ?ALL_TYPES],
    
    timer:sleep(1200),
    _ = [{records, 0} = jc:map_size(W) || W <- ?ALL_TYPES],
    
    {error, badarg} = bridge({put, amap, k, v, -2}),

    jc:flush(),

    _ =[begin 
	    V = K * 2 ,
	    {ok, K} = bridge({put_s, Map, K, V, 1}),
    {ok, V} = bridge({get, Map, K})
	end || K <- lists:seq(1,100)],
    
    _ = [begin
	     {ok, W} = bridge({put_s, W, W, W, 1, 1}),
	     {ok, W} = bridge({get, W, W}),
	     {records, 1} = jc:map_size(W)
	 end || W <- ?ALL_TYPES],
    
    timer:sleep(1200),
    _ = [{records, 0} = jc:map_size(W) || W <- ?ALL_TYPES],
    
    {error, badarg} = bridge({put_s, amap, k, v, 2, -2}),
    {error, badarg} = bridge({put_s, amap, k, v, -2, 2}).

    

% with monotonically increacing sequences, behaves just like put
% with a break in sequence, put has no effect
% sequence numbers must be greater than 0
put_seq_test(_Config) ->
    Map = bed,
    _ =[begin 
	    V = K * 2 ,
	    {ok, K} = bridge({put_s, Map, K, V, K}),
	    {ok, V} = bridge({get, Map, K})
	end || K <- lists:seq(1,100)],
    
    _ = [begin
	     {ok, W} = bridge({put_s, W, W, W, 1, 1}),
	     {ok, W} = bridge({get, W, W}),
	     {records, 1} = jc:map_size(W)
	 end || W <- ?ALL_TYPES],
    
    timer:sleep(1200),
    _ = [{records, 0} = jc:map_size(W) || W <- ?ALL_TYPES],
    jc:clear(Map),
    {ok, 10} = bridge({put_s, Map, 10, 10, 10}),
    {ok, 10} = bridge({put_s, Map, 10, 20, 20}),
    {ok, 10} = bridge({put_s, Map, 10, "20 2", 21}),
    {error, out_of_seq} = bridge({put_s, Map, 10, 15, 15}),
    timer:sleep(300),
    {ok, "20 2"} = bridge({get, Map, 10}),

    {error, badarg} = bridge({put_s, amap, k, v, -1}),
    {error, badarg} = bridge({put_s, amap, k, v, 200, -2}).


						% TTL should evict in appropriate seconds, 
						% get should NOT extend ttl
						% updating a record with one that does NOT have a ttl should cancel the TTL
put_eviction_test(_Config)->
    K = 123,
    M = bed,
    V  = <<"some binary string value">>,
    {ok, K} = bridge({put, M, K, V, 3}),
    timer:sleep(500),

    RecRef = mnesia:dirty_first(ttl),
    [#ttl{ttl_secs=3, timer_ref=TR}] = mnesia:dirty_read({ttl, RecRef}),

    timer:sleep(1500),
    {ok, V} = bridge({get, M, K}),
    [#ttl{ttl_secs=3, timer_ref=TR2}] = mnesia:dirty_read({ttl, RecRef}),
    true = (TR == TR2),

    timer:sleep(2000),
    miss = bridge({get, M, K}),

    [] = mnesia:dirty_read({ttl, RecRef}),

    bridge({put, bed, 1, 1, 2}),
    bridge({put, bed, 1, 1}),
    timer:sleep(3000),
    {ok, 1} = bridge({get, bed, 1}).


						% only good tuples get put, 
						% only existing keys get got
						% evictions happen if ttl is set
						% ditto for put_all_s
put_all_test(_Config)->
    KVs = [{K, K*2} || K <- lists:seq(1,100)],
    Ks = [K || {K,_} <- KVs],
    Vs = [V || {_,V} <- KVs],


    {ok, 100} = bridge({put_all, bed, [{bad, bad, bad, bad},{bad}|KVs]}),
    {ok, {Hits, [missing]}} = bridge({get_all, bed, [missing|Ks]}),
    KVs = lists:reverse(Hits),

    {ok, 100} = bridge({put_all, bed, KVs, 2}),
    {ok, TestVs} = bridge({values, bed}),
    true = (lists:sort(Vs) == lists:sort(TestVs)),
    {ok, TestKs} = bridge({key_set, bed}),
    true = (lists:sort(Ks) == lists:sort(TestKs)),

    timer:sleep(2100),
    {ok, {[], M}} = bridge({get_all, bed, Ks}),
    Ks = lists:reverse(M),

    {ok, 0} = bridge({put_all, bed,[]}),

    jc:flush(),
    KVs2 = [{K, K*2} || K <- lists:seq(1,100)],
    Ks2 = [K || {K,_} <- KVs2],


    {ok, 100} = bridge({put_all_s, bed, [{bad, bad, bad, bad},{bad}|KVs2], 100}),
    {ok, {Hits, [missing]}} = bridge({get_all, bed, [missing|Ks2]}),
    KVs2 = lists:reverse(Hits),

    jc_s:put(bed, 10, 10, 10),
    {ok, 20} = bridge({get, bed, 10}),

    {error, out_of_seq} = bridge({put_all_s, bed, [{bad, bad, bad, bad},{bad}|KVs2], 1}),
    jc:flush(),
    
    {ok, 100} = bridge({put_all_s, bed, [{bad, bad, bad, bad},{bad}|KVs2], 3, 100}),
    bridge({put_all_s, bed, [{bad, bad, bad, bad},{bad}|KVs2], 1, 99}),
    timer:sleep(2000),
    {records, 100} = jc:map_size(bed),
    timer:sleep(1100),
    {records, 0} = jc:map_size(bed).


% Evicting removes appropriate keys.
% Ditto for evict_s if sequence is correct
evict_simple_test(_Config)->
    KVs = [{K, K*2} || K <- lists:seq(1, 100)],
    {ok, 100} = jc:put_all(bed, KVs),
    {ok, 100} = jc:put_all(evs, KVs),

    [bridge({evict, bed, K}) || K <- lists:seq(1, 100, 2)],

    Misses = [jc:get(bed, K) || K <- lists:seq(1,100, 2)],
    Hits = [jc:get(bed, K) || K <- lists:seq(2,100, 2)],

    [] = lists:dropwhile(fun(Elem) -> Elem == miss end, Misses),
    [] = lists:dropwhile(fun(Elem) -> Elem /= miss end, Hits),

    {ok, Result} = jc:values(evs),
    100 = length(Result),

    jc:flush(),

    {ok, 100} = bridge({put_all_s, bed, KVs, 10}),
    {ok, 100} = bridge({put_all_s, evs, KVs, 9}),


    [bridge({evict_s, bed, K, 1}) || K <- lists:seq(1, 100, 2)],
    [bridge({evict_s, evs, K, 10}) || K <- lists:seq(1, 100, 2)],
    {records, 100} = jc:map_size(bed),
    [bridge({evict_s, bed, K, 11}) || K <- lists:seq(1, 100, 2)],


    Misses = [jc:get(bed, K) || K <- lists:seq(1,100, 2)],
    Hits = [jc:get(bed, K) || K <- lists:seq(2,100, 2)],

    [] = lists:dropwhile(fun(Elem) -> Elem == miss end, Misses),
    [] = lists:dropwhile(fun(Elem) -> Elem /= miss end, Hits),

    {ok, R2} = jc:values(evs),
    50 = length(R2).


% mnesia indexes on the i1 columns should only be there when something is being
% indexed
% indexes on two different maps can share a position
% indexe paths are converted to binary
% no more than 4 indexes per map
% adding an index flushes the map
indexing_test(_Config) ->

    [] = jc_store:indexes(),
    false = 
	lists:member(#key_to_value.i1, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i2, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i3, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i4, mnesia:table_info(key_to_value, index)),
    
    ok = bridge({start_indexing, bed, "menu.id"}),
    [{{bed, {<<"menu">>,<<"id">>}}, #key_to_value.i1}] = bridge({indexes}),
    [{{bed, {<<"menu">>,<<"id">>}}, #key_to_value.i1}] = bridge({indexes, bed}),
    
    ok = bridge({start_indexing, bed, "menu.value"}),
    A = lists:sort([{{bed, {<<"menu">>,<<"value">>}}, #key_to_value.i2},
		    {{bed, {<<"menu">>,<<"id">>}}, #key_to_value.i1}]),

    A = lists:sort(jc_store:indexes()),

    ok = bridge({start_indexing, other, "menu.value"}),
    
    B = lists:sort([{{bed, {<<"menu">>,<<"value">>}}, #key_to_value.i2},
		    {{bed, {<<"menu">>,<<"id">>}}, #key_to_value.i1},
		    {{other, {<<"menu">>,<<"value">>}}, #key_to_value.i1}]),

    B = lists:sort(jc_store:indexes()),

    true = 
	lists:member(#key_to_value.i1, mnesia:table_info(key_to_value, index)),
    true = 
	lists:member(#key_to_value.i2, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i3, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i4, mnesia:table_info(key_to_value, index)),
    
    ok = bridge({stop_indexing, bed, "menu.id"}),
    ok = bridge({stop_indexing, bed, "menu.value"}),
    
    [{{other, {<<"menu">>,<<"value">>}}, #key_to_value.i1}] = bridge({indexes}),
    [{{other, {<<"menu">>,<<"value">>}}, #key_to_value.i1}] = bridge({indexes, other}),
    [] = jc_store:indexes(bed),
    true = 
	lists:member(#key_to_value.i1, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i2, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i3, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i4, mnesia:table_info(key_to_value, index)),
    ok = bridge({stop_indexing, other, "menu.value"}),
    [] = jc_store:indexes(),
    false = 
	lists:member(#key_to_value.i1, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i2, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i3, mnesia:table_info(key_to_value, index)),
    false = 
	lists:member(#key_to_value.i4, mnesia:table_info(key_to_value, index)),

    ok = bridge({start_indexing, bed, "menu.id1"}),
    ok = bridge({start_indexing, bed, "menu.id2"}),
    ok = bridge({start_indexing, bed, "menu.id3"}),
    ok = bridge({start_indexing, bed, "menu.id4"}),
    ok = bridge({start_indexing, other, "menu.id1"}),
    ok = bridge({start_indexing, other, "menu.id2"}),
    ok = bridge({start_indexing, other, "menu.id3"}),
    ok = bridge({start_indexing, other, "menu.id4"}),
    ok = bridge({stop_indexing, bed, "menu.id4"}),
    ok = bridge({start_indexing, bed, "menu.id4"}),

    ok = bridge({start_indexing, bed, "menu.id4"}),
    ok = bridge({start_indexing, bed, "menu.id4"}),
    {error, no_indexes_available} = bridge({start_indexing, bed, "menu.id44"}),
    ok = bridge({start_indexing, other, "menu.id4"}),
    ok = bridge({start_indexing, other, "menu.id4"}),
    {error, no_indexes_available} = bridge({start_indexing, other, "menu.id44"}),

    jc:put(bed,1,1),
    jc:put(evs,1,1),
    ok = bridge({stop_indexing, bed, "menu.id4"}),
    {records, 1} = jc:map_size(bed),
    ok = bridge({start_indexing, bed, "menu.id4"}),
    {records, 0} = jc:map_size(bed),
    {records, 1} = jc:map_size(evs),

    ok = bridge({stop_indexing, bed, "menu.id1"}),
    ok = bridge({stop_indexing, bed, "menu.id2"}),
    ok = bridge({stop_indexing, bed, "menu.id3"}),
    ok = bridge({stop_indexing, bed, "menu.id4"}),
    ok = bridge({stop_indexing, other, "menu.id1"}),
    ok = bridge({stop_indexing, other, "menu.id2"}),
    ok = bridge({stop_indexing, other, "menu.id3"}),
    ok = bridge({stop_indexing, other, "menu.id4"}).



% evict_ and values_match should work with indexes on
% non indexed fields should work as well
% values MUST be binary
indexed_match_test(Config) ->
    ok = bridge({start_indexing, bed, "menu.popup.menuitem.2.value"}),
    ok = bridge({start_indexing, bed, "menu.id"}),
    ok = bridge({stop_indexing, bed, "menu.value"}),
    evict_match_test(Config),
    values_match_test(Config),



    jc:put(bed, 1, "not json"),
    jc:put(bed, 2, true),
    V = <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open2\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>,

    jc:put(bed, file, V),
    {ok, [{file, V}]} = bridge({values_match, bed, "menu.id=\"file\""}),
    {ok, [{file, V}]} = bridge({values_match, bed, "menu.value=true"}),


    {ok, [{file, V}]} = 
	bridge({values_match, bed, "menu.popup.menuitem.2.value=\"Open2\""}),
    bridge({stop_indexing, bed, "menu.popup.menuitem.2.value"}),
    {ok, [{file, V}]} = 
	bridge({values_match, bed, "menu.popup.menuitem.2.value=\"Open2\""}),

    {ok, []} = bridge({values_match, bed, "menu.nothere=\"file\""}),

    ok = bridge({evict_match, bed, "menu.id=\"file\""}),
    {ok, []} = bridge({values_match, bed, "menu.id=\"file\""}),

    jc:put(bed, file, V),
    {ok, [{file, V}]} = bridge({values_match, bed, "menu.value = true"}),
    ok = bridge({evict_match, bed, "menu.value=true"}),
    miss = bridge({get, bed, file}),
    {ok, "not json"} = jc:get(bed, 1),
    {ok, true} = jc:get(bed, 2),
    ok = bridge({stop_indexing, bed, "menu.id"}),
    jc:put(bed, file, V),
    {ok, [{file, V}]} = bridge({values_match, bed, "menu.id = \"file\""}).





% evict should only evict what matches
% evict_all sould evict from all maps
% match criteria can have white space and be binary
% always returns true
% can remove multiple items
% values can be binary
evict_match_test(_config) ->
    [jc:put(bed, X,	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}") || X <- lists:seq(1,1000)],

    jc:put(bed, file, <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>),

    ok = bridge({evict_match, bed, "menu.id=\"file\""}),
    miss = jc:get(bed, file),
    {records, 1000} = jc:map_size(bed),

    {ok, _V} = jc:get(bed, 22),
    ok = bridge({evict_match, bed, <<"  menu.id  =   22">>}),
    miss = jc:get(bed, 22),
    {records, 999} = jc:map_size(bed),

    true = bridge({contains_key,bed, 10}),
    false = bridge({contains_key, bed, file}),
    ok = bridge({evict_match, bed, "a cow jumped over the moon"}),

    ok = bridge({evict_match, bed, "menu.value=\"File\""}),
    {records, 0} = bridge({map_size, bed}),

    [jc:put(bed, X,
	    "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}") || X <- lists:seq(1,200)],

    [jc:put(evs, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}") || X <- lists:seq(1,200)],
    [jc:put(trx, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}") || X <- lists:seq(1,200)],

    jc:put(bed, file, <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>),

    ok = bridge({evict_all_match, "menu.id=\"file\""}),
    miss = jc:get(bed, file),
    ok = bridge({evict_all_match, "menu.id=20"}),
    miss = jc:get(bed, 20),
    miss = jc:get(evs, 20),
    miss = jc:get(trx, 20),
    {records, 199} = bridge({map_size, bed}),
    {records, 199} = bridge({map_size, evs}),
    {records, 199} = bridge({map_size, trx}).


% evict should only evict what matches
% evict_all sould evict from all maps
% match criteria can have white space and be binary
% always returns true
% can remove multiple items
% values can be binary
evict_match_s_test(_config) ->
    [jc_s:put(bed, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}", 100) || X <- lists:seq(1,1000)],

    jc_s:put(bed, file, <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>, 100),

    {error, out_of_seq} = bridge({evict_match_s, bed, "menu.id=\"file\"",1}),
    {ok, _} = jc:get(bed, file),
    ok = bridge({evict_match_s, bed, "menu.id=\"file\"",101}),
    miss = jc:get(bed, file),
    {records, 1000} = jc:map_size(bed),

    {ok, _V} = jc:get(bed, 22),
    {error, out_of_seq} = bridge({evict_match_s, bed, <<"  menu.id  =   22">>,100}),
    ok = bridge({evict_match_s, bed, <<"  menu.id  =   22">>,102}),
    miss = jc:get(bed, 22),
    {records, 999} = jc:map_size(bed),

    true = bridge({contains_key,bed, 10}),
    false = bridge({contains_key, bed, file}),
    ok = bridge({evict_match_s, bed, "a cow jumped over the moon", 103}),

    ok = bridge({evict_match_s, bed, "menu.value=\"File\"", 104}),
    {records, 0} = bridge({map_size, bed}),

    jc:flush(),
    [jc_s:put(bed, X,
	    "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}", 100) || X <- lists:seq(1,200)],

    [jc_s:put(evs, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}", 200) || X <- lists:seq(1,200)],
    [jc_s:put(trx, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}",400) || X <- lists:seq(1,200)],

    jc_s:put(bed, file, <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>, 110),

    ok = bridge({evict_all_match_s, "menu.id=\"file\"", 1}),
    {ok, _} = jc:get(bed, file),
    ok = bridge({evict_all_match_s, "menu.id=\"file\"", 121}),
    miss = jc:get(bed, file),

    ok = bridge({evict_all_match_s, "menu.id=20",200}),
    {records, 199} = bridge({map_size, bed}),
    {records, 199} = bridge({map_size, evs}),
    {records, 200} = bridge({map_size, trx}),

    ok = bridge({evict_all_match_s, "menu.id=20", 500}),
    miss = jc:get(bed, 20),
    miss = jc:get(evs, 20),
    miss = jc:get(trx, 20),
    {records, 199} = bridge({map_size, bed}),
    {records, 199} = bridge({map_size, evs}),
    {records, 199} = bridge({map_size, trx}).






% values_match returns what it should match
% criteria can have white space and be binary
% always returns {ok, List}
values_match_test(_config) ->
    [jc:put(bed, X,
	     "{\"menu\": { \"id\": " ++ integer_to_list(X) ++ ", \"value\": \"File\", \"popup\": {\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}, {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} ]}}}") || X <- lists:seq(1,1000)],

    V = <<"{\"menu\": { \"id\": \"file\",  \"value\": true,  \"popup\": {    \"menuitem\": [      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}    ]  }}}">>,

    jc:put(bed, file, V),

    {ok, [{file, V}]} = bridge({values_match, bed, "menu.id=\"file\""}),
    ok = jc:evict_match(bed, <<"menu.id    = \"file\"  ">>),  
    {ok, []} = bridge({values_match, bed, "menu.id=\"file\""}),
    {ok, Vs} = bridge({values_match, bed, "menu.value=\"File\""}),
    1000 = length(Vs),
    {ok, []} = bridge({values_match, bed, "a cow jumped over the moon"}),
    {ok, []} = bridge({values_match, bed, "menu.id= a cow jumped over the moon"}),
    true.



% return all {k,v} that were removed
remove_items_test(_config) ->
    {ok,3} = jc:put_all(bed, [{1, one},{2, two},{3, three}]),
    {ok,[{1,one}]} = bridge({remove_items, bed, [1, 22]}),
    {ok, []} = bridge({remove_items, bed, [1, 22]}),
    {ok, Result} = jc:key_set(bed),
    [2,3] = lists:sort(Result),
    {ok,[{3, three}, {2, two}]} = bridge({remove_items, bed, [2, 3, 3, 4]}),
    {records, 0} = jc:map_size(bed), 
    jc:flush(),

    {ok,3} = jc_s:put_all(bed, [{1, one},{2, two},{3, three}], 10),
    {ok,[{1,one}]} = bridge({remove_items_s, bed, [1, 22], 11}),
    {ok, []} = bridge({remove_items_s, bed, [1, 22], 12}),
    {ok, Result} = jc:key_set(bed),
    true = ([2,3] == lists:sort(Result)),
    {error, out_of_seq} = bridge({remove_items_s, bed, [2, 3, 3, 4], 1}),
    {ok,[{3, three}, {2, two}]} = bridge({remove_items_s, bed, [2, 3, 3, 4], 111}),
    {records, 0} = jc:map_size(bed).


% clear all data asoociates with the map
% only clear the given map
clear_test(_config) ->
    [jc:put(bed, X, X) || X <- lists:seq(1,10)],
    [jc:put(evs, X, X) || X <- lists:seq(1,10)],
    bridge({clear, bed}),
    {records, 0} = jc:map_size(bed),
    {records, 10} = jc:map_size(evs).



% adding a map max_ttl with a 0 time removes the ttl
% adding a map max_ttl that doesn't exist adds it
% adding a map max_ttl that exists overwirtes it
% adding a bad map max_ttl returns an error but doesn't change anything
max_ttl_test(_Config) ->
    [] = bridge({get_max_ttls}),
    ok = bridge({set_max_ttl, m, 10}),
    [{m, 10}] = bridge({get_max_ttls}),
    ok = bridge({set_max_ttl, m, 20}),
    {error, badarg} = bridge({set_max_ttl, m, a}),
    [{m, 20}] = bridge({get_max_ttls}),
    ok = bridge({set_max_ttl, m, 0}),
    [] = bridge({get_max_ttls}).


% map ttl removes only appropriate items
max_ttl_evict_test(_Config) ->
    jc_eviction_manager ! {max_ttl_evict, 1},
    ok = bridge({set_max_ttl, bed, 5}),
    jc:put(bed, short, 10),
    jc:put(evs, akey, 10),
    {ok,10} = jc:get(bed, short),
    {ok, 10} = jc:get(evs, akey),
    timer:sleep(2000),
    jc:put(bed, long, 10),
    timer:sleep(4500),
    miss = jc:get(bed, short),
    {ok, 10} = jc:get(evs, akey),
    {ok, 10} = jc:get(bed, long),
    timer:sleep(2000),
    miss = jc:get(bed, long),
    {ok, 10} = jc:get(evs, akey),
    jc_eviction_manager ! {max_ttl_evict, 120},
    true.
    
    


% delete_map should only remove items older than parameter
% bad parameter should still return okay
delete_map_since_test(_config)->
    {ok, a} = jc:put(bed, a, "A"),
    {ok,"A"} = jc:get(bed, a),
    ok = bridge({evict_map_since, bed, 2}),
    {ok, "A"} = jc:get(bed, a),
    timer:sleep(2000),
    ok = bridge({evict_map_since, bed, 2}),
    miss = jc:get(bed, a),
    ok = bridge({evict_map_since, bed, crap}).


% put the pid in ps_client and appropriate subscription in ps_sub
% receive the correct subscription messages
% unsubscribe removes Pid from ps_client and ps_sub
% stop getting messages when unsubscribed
map_subscribe_test(_Config) ->
    jc_bridge ! {self(), {map_subscribe, bed, key, write}}, 
    timer:sleep(100),
    This = self(),
    true = lists:member(This, mnesia:dirty_all_keys(ps_client)),
    A = mnesia:dirty_read(ps_sub, mnesia:dirty_next(ps_sub, mnesia:dirty_first(ps_sub))),
    case A of
        [{ps_sub, {map_sub, bed, key, write}, Set}] -> 
            sets:is_element(self(), Set);
        _ ->
            [{ps_sub, {map_sub, bed, key, write}, Set2}]  = 
                mnesia:dirty_read(ps_sub, mnesia:dirty_first(ps_sub)),
            sets:is_element(self(), Set2)
    end,

    2 = jc_psub:client_count(),
    4 = jc_psub:load(),

    jc_bridge ! {self(), {put, bed, key, 1}}, 
    jc_bridge ! {self(), {put, bed, 1, 1}}, 
    jc_bridge ! {self(), {evict, bed, key}}, 
    jc_bridge ! {self(), {put, bed, otherkey, 2}}, 

    true = receive
	       {map_event, {bed, 1, 1}} ->
		   false;
	       {map_event, {bed, key, delete}} ->
		   false;
	       {map_event, {bed, otherkey, _, _}} ->
		   false;
	       {map_event, {bed, key, write, 1}} ->
		   true
	   end,

    jc_bridge ! {self(), {put, bed, key, 2}}, 
    true = receive {map_event, {bed, key, write, 2}} -> true after 100 -> false end,

    jc_bridge ! {self(), {map_unsubscribe, bed, key, write}}, 
    timer:sleep(200),
    jc_psub ! {evict_deadbeats, 120000},
    timer:sleep(200),

    1 = length(mnesia:dirty_all_keys(ps_client)),
    1 = length(mnesia:dirty_all_keys(ps_sub)),	

    2 = jc_psub:load(),
    1 = jc_psub:client_count(),


    flush(),

    jc:put(bed, otherkey, 1),
    jc:put(bed, key, 1),

    true = receive Oops -> Oops after 100 -> true end,


    jc_bridge ! {self(), {map_subscribe, bed, key, delete}}, 
    jc_bridge ! {self(), {map_subscribe, evs, any, any}}, 
    timer:sleep(600),
    
    Zeroth = mnesia:dirty_first(ps_sub),
    First = mnesia:dirty_next(ps_sub, mnesia:dirty_first(ps_sub)),

    Second = mnesia:dirty_next(ps_sub, First),


    try 
        [{ps_sub, {map_sub, evs, any, any}, X1}] = 
            mnesia:dirty_read(ps_sub, First),
        sets:is_element(self(), X1)
    catch
        _:_ ->
        [{ps_sub, {map_sub, evs, any, any}, X2}] = 
                mnesia:dirty_read(ps_sub, Second),
            sets:is_element(self(), X2)
    end,

    try [{ps_sub, {map_sub, bed, key, delete}, Y}] = 
             mnesia:dirty_read(ps_sub, Second),
            sets:is_element(self(), Y)
    catch
        _:_ ->
            [{ps_sub, {map_sub, bed, key, delete}, Y2}] = 
                mnesia:dirty_read(ps_sub, Zeroth),

            sets:is_element(self(), Y2)
    end,

   
    timer:sleep(400),
    jc:put(bed, otherkey, 2),
    jc:put(bed, key, 2),     % put causes evict of previous value
    jc:put(bed, key, 3),   
    true = receive
	       {map_event, {bed, otherkey, delete}} ->
		   false;
	       {map_event, {bed, key, delete}} ->
		   receive {map_event, {bed, key, delete}} -> true after 200 -> false end
	   after 800 -> false_false
	   end,

    jc:put(evs, one, 1),
    true = receive {map_event, {evs, one, write, 1}} -> true end,
    jc:put(evs, one, 2),
    true = receive {map_event, {evs, one, delete}} ->
		   receive {map_event, {evs, one, write, 2}} ->
			    true
		   end
	   end.



topic_subscribe_test(_Config)->
    Me = self(),
    _ = spawn(fun() -> bouncer(Me, {topic_subscribe, test, any}) end),
    up = collect(),
    ok = bridge({topic_unsubscribe, test, any}),
    ok = bridge({topic_event, test, a_test}),
    {topic_event, {test, a_test}} = collect().


% subscribing to node_events should get them
% only one event per event
% stopping jc or killing the node should cause a node_down
cluster_test(_Config) ->
    jc:stop(),
    application:stop(ranch),
    mnesia:stop(),
    erlang:open_port({spawn, "erl -name jc2@127.0.0.1 -config ../../lib/jc/test/app.config -eval 'application:ensure_all_started(jc)' -pa ../../lib/*/ebin"},[out]),
    timer:sleep(1000),
    application:ensure_all_started(jc),
    timer:sleep(2000),
    ok = bridge({node_topic_sub}),

    
    erlang:open_port({spawn, "erl -name jc3@127.0.0.1 -config ../../lib/jc/test/app2.config -eval 'application:ensure_all_started(jc)' -pa ../../lib/*/ebin"},[out]),
    timer:sleep(2000),
    {topic_event, {jc_node_events,{nodeup,'jc3@127.0.0.1',
				   ['jc1@127.0.0.1','jc2@127.0.0.1',
				    'jc3@127.0.0.1'],
				   ['jc1@127.0.0.1','jc2@127.0.0.1',
				    'jc3@127.0.0.1']}}} = collect(),


    jc_psub:topic_subscribe(self(), topic_test, any),
    jc_psub:topic_subscribe(self(), topic_test, any),
    jc_psub:topic_subscribe(self(), topic_test2, any),
    jc_psub:map_subscribe(self(), bed, any, write),
    jc_psub:map_subscribe(self(), bed, any, write),
    jc_psub:map_subscribe(self(), bed, any, delete),
    
    rpc:call('jc2@127.0.0.1', jc, put, [bed, 1, 1]),
    {map_event, {bed,1,write,1}} = collect(),
    rpc:call('jc3@127.0.0.1', jc, put, [bed, 1, 2]),
    {map_event, {bed,1,delete}} = collect(),
    {map_event, {bed,1,write,2}} = collect(),
    rpc:call('jc2@127.0.0.1', jc_psub, topic_event, [topic_test, tt1]),
    {topic_event, {topic_test,tt1}} =collect(),
    rpc:call('jc3@127.0.0.1', jc_psub, topic_event, [topic_test2, tt2]), 		    
    {topic_event, {topic_test2,tt2}} =collect(),
    collect_error = collect(),
    
    rpc:call('jc2@127.0.0.1', init, stop, []),

    {topic_event, {jc_node_events,{nodedown,'jc2@127.0.0.1',
				   ['jc1@127.0.0.1', 'jc3@127.0.0.1'],
				   ['jc1@127.0.0.1','jc2@127.0.0.1', 'jc3@127.0.0.1']}}} = 
	collect(),

    rpc:call('jc3@127.0.0.1', jc, evict, [bed, 1]),
    {map_event, {bed,1,delete}} = collect(),


    rpc:call('jc3@127.0.0.1', jc, stop, []),

    {topic_event, {jc_node_events,{nodedown,'jc3@127.0.0.1',
				   ['jc1@127.0.0.1'],
				   ['jc1@127.0.0.1','jc2@127.0.0.1', 'jc3@127.0.0.1']}}} = 
	collect(),
   collect_error = collect(),


    rpc:call('jc3@127.0.0.1', application, ensure_all_started, [jc]),
    {topic_event, {jc_node_events,{nodeup,'jc3@127.0.0.1',
				   ['jc1@127.0.0.1','jc3@127.0.0.1'],
				   ['jc1@127.0.0.1','jc2@127.0.0.1',
				    'jc3@127.0.0.1']}}} = collect(),
    ok = bridge({node_topic_unsub}),
    timer:sleep(100),
    rpc:call('jc3@127.0.0.1', jc, stop, []),
    collect_error = collect().




collect() ->
    receive
	Msg ->
	    Msg
    after 
	2000 ->
	    collect_error
    end.
		  
	
bouncer(Pid, Pattern) ->
    jc_bridge ! {self(), Pattern},
    receive 
	ok -> 
	    Pid ! up,
	    loop(Pid)
    after 
	1000 ->
	    loop(Pid)
    end.


loop(Pid)->
    receive
	X ->
	    Pid ! X
    after
	5000 ->
	    Pid ! error
    end.


% Convenience function: use jc_bridge for many test cases. 
%
bridge(Pattern) ->
    jc_bridge ! {self(), Pattern},
    receive
	Answer ->
	    Answer
    after
	1000 ->
	    error
    end.



flush() ->
    receive
	_ ->
	    flush()
    after
	0 ->
	    ok
    end.

% Try to used erlang 18+ timestamp(), support older versions if necessary.
timestamp() ->
    try
	erlang:timestamp()
    catch
	error:undef ->
	    erlang:now()
end.
