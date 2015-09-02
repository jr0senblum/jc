JC
====

##Erlang, Distributable, In-Memory Cache with Pub/Sub,  Serialization and JSON-Query Support.


###Features
* Cache entries are Map, Key, Value, [TTL], [Sequence]
  * Maps represent a name-space for Keys - similar to the notion
    of 'bucket'
    in other caching systems
  * {Map, Key} must be unique	
  * Maps, Keys and Values can be any Erlang term
  * TTL is time-to-live in seconds
* Consistency through Serialization: An alternative API allows
    for a sequence-number parameter on the put/x, evict/x, match/x 
    and remove/x opperations. Operations whose sequence number is
    lower than the current (per map) max are disallowed thereby
    ensuring, for example, that stale puts do not overwrite 
    "fresh" ones
 *  JSON Query Support
     * Query by JSON: When Values are JSON, evict_match/2,
       evict_all_match/1 and values_match/2 can search or evict
       keys whose values match a java-style, dot-path, string:
       "id.type=3"
    * Ad-hoc, Index Support: In order to support faster
      operations, (2-3 orders of magnitude), each map can have up to 4,
       dot-path, strings configured for which jc will create index
       support.
    * Auto Index Recognition - Ability to detect frequently used JSON querries
      and automatically start indexing on them.
* User Controlled Eviction
  * Map-level TTL: A job runs at configured intervals and removes
  items whose create-date is older than a map-specific, configured 
  number of seconds
  * Item-level TTL: PUTS can include a TTL which defines when the
  item should be evicted. Used for shorter TTLs, as an exception
  to a Map-level TTL, or when more precision is required than 
  offered by the Map-level TTL.
* Pub/Sub 
  * Clients can subscribe to Map events for a specific key or
  any key and for write, delete or either operations
  * Clients can create and subscribe to arbitrary 'topic's and 
  broadcast arbitrary messages under those topic names
* Bridge process that accepts messages from a client indicating
  cache operations, executes the cache operations and returns the
  results to the client. This has been used with JInterface to 
  interoperate with CLOJURE and Java clients
* Fine-grained logging via lager



###Cache Functions (jc)
* PUT
  * put(Map, Key, Value) -> {ok, {key, Key}} | {error, Term}
  * put(Map, Key, Value, TTLSescs) -> {ok, {key, Key}} | 
                                      {error, Term}
  * put_all(Map, [{K,V},{K,V},...]) -> {ok, No_Successful_Puts}
  * put_all(Map, [{K,V},{K,V},...], TTLSecs) -> {ok, No_Successful_Puts}
* DELETE
  * clear(Map) -> ok
  * delete_map_since(Map, Age_In_Secs) -> ok.
  * evict(Map, Key) -> ok
  * evict_match(Map, Criteria = "Json.Path.Match=Value") -> ok
  * evict_all_match(Criteria = "Json.Path.Match=Value") -> ok
  * flush() -> ok
  * flush(silent) -> ok <-- Don't send alerts to subscribers
  * remove_items(Map, Keys) -> {ok, [{K, V}, ...]} for each Key
  removed.
* GET
  * contains_key(Map, Key) -> true | false.
  * get(Map, Key) -> {ok, Value} | miss
  * get_all(Map, [K1, K2, ...]) -> {ok, {Found=[{K1,V1},...],
                                         Misses=[K2,...]}}
  * key_set(Map) -> {ok, [K1, K2, ...]}
  * values(Map) -> {ok, [V1, V2, ...]}
  * values_match(Map, Criteria="JSon.Path.Match=Value") ->
                                     {ok, [{K1,V1}, {K2,V2}, ...]}
* Meta
  * cache_nodes() -> {nodes, {active, [Node1,... ]}, 
                             {configured, [Node1,... ]}}
  * cache_size() -> {size, [{TableName, RecordCnt, Words}],...}
  * map_size(Map) -> {records, Count}
  * maps() -> [Map1, Map2,...]
  * up() -> {uptime, [{up_at, String},{now, String},
                      {up_time {D, {H, M, S}}}]


### Searializable Cache Functions (jc_s)
* trx_ret() :: {error, badarg | out_of_seq | term}
* Put
  * put(Map, Key, Value, Seq) -> {ok, {key, Key}} | trx_ret
  * put(Map, Key, Value, TTLSecs, Seq) -> {ok, {key, Key}} |
                                          trx_ret
  * put_all(Map, [{K,V},{K,V},...], Seq) -> {ok,
   No_Successful_Puts} | trx_ret
  * put_all(Map, [{K,V},{K,V},...], TTLSecs, Seq) -> {ok,
   Num_Successful_Puts} | trx_ret
* Delete
  * evict(Map, Key, Seq) -> ok | trx_ret
  * evict_match(Map, Criteria = "Json.Path.Match=Value", Seq) ->
   ok | trx_ret
  * evict_all_match(Criteria = "Json.Path.Match=Value", Seq) ->
   ok | trx_ret
  * remove_items(Map, Keys, Seq) -> {ok, [{K, V}, ...]} | | 
                                   trx_ret
* Meta
  * sequence() -> {ok, [{Map, Highest_Number},... ]}
  * sequence(Map) -> {ok, Hightest_Number} | {ok, not_exist}



###Eviction Manager Functions (jc_eviction_manager)
* set_max_ttl(Map, Secs) -> ok | {error, badarg}
* get_max_ttls() -> [{Map, Secs}, ...]


###Pub/Sub Functions (jc_psub)
* map_subscribe(Pid, Map, Key|any, write|delete|any) -> ok |
                                                   {error, badarg}
  * client will receive
    * {map, key, delete}, or
    * {map, key, write, value}
* map_unsubscribe(Pid, Map, Key|any, write|delete|any) -> ok |
                                                   {error, badarg}
* topic_subscribe(Pid, Topic, Value) -> ok | {error, badarg}
  * client will receive {Topic, Value}
* topic_unsubscribe(Pid, Topic, Value) -> ok | {error, badarg}
* topic_event(Topic, Value) -> ok. <-- Broadcasts Value to all
  subscribers of Topic


###Indexing Functions (jc_store)
  * start_indexing(Map, Path={bed,"menu.id"}) -> ok |
                                  {error, no_indexes_available} |
							       {error, Term}
  * stop_indexing(Map, Path={bed,"menu","id"}) -> ok
  * indexes(Map) -> [{{Map, Path}, Position},...] for all indexes
                                                  of given Map
  * indexes() -> [{{Map, Path}, Position},...] for all indexes


###Bridge Functions (jc_bridge)
 * All functions from the jc, jc_s, jc_eviction_manager, jc_psub
 and jc_store are supported and are of the form:
 
   `{From, {Fn, P1, P2,...}}`
   
    as in 
    
    `jc_bridge ! {Pid, {put, Map, Key, Value}}`
    
* Additionally, 

  {From, {node_topic_sub}} -> ok | {error, badarg}, 
  client will recieve:
   
   `{jc_node_events, {nodedown, DownedNode, [ActiveNodes],[ConfiguredNodes]}}`
   
    or

    `{jc_node_events, {nodeup, UppedNode, [ActiveNodes],[ConfiguredNodes]}}`

  {From, {node_topic_unsub}} -> ok.


###Application Modules
* jc_cluster
  * Simple, mnesia-based, cluster creation and management
* jc, jc_s, jc_store, jc_eviction_manager
  * Caching operations, Key-specific and Map-level TTLs
* jc_sequence
  * Singleton service enforcing strictly monotonic sequence 
  numbers on jc_s operations
* jc_analyzer
  * Analysis and indexing inititation of JSON query strings
* jc_psub: 
  * Pub / Sub of cache write and delete events
  * On-demand, ad-hoc topic events
  * Predefined jc_node_events topic for node-up and node-down
   notifications
* jc_bridge
  * Server that acts as a proxy between an external process and
  j_cache functionality




###Configuration
* Application configuration is in sys.config which is heavily
commented
* Cookie, node-name and auto-restart of VM controlled by vm.args


###Build Instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash


      [root@db01] git clone http://jrosenblum@cleng01.statcom.local:7990/stash/scm/plym/j-cache.git
      [root@db01] cd j_cache`

* For an environment which does NOT use NIF jsonx nor native compile (i.e., windows)

      [root@db01] ./rebar get-deps clean compile -C rebar_windows.config


* For an environment which uses the NIF jsonx and native compilation
  
      [root@db01] ./rebar get-deps clean compile
    
* 
* Edit rel/file/sys.config and vm.args
   * vm.args: Indicate the correct node names and cookie in vm.args
   * vm.args: FOR WINDOWS, comment out the node name line

           # -name jcache@127.0.0.1.
   * sys.config: Adjust prarameters as neccesary.
   * sys.config: FOR WINDOWS, comment out console-logging
   
           %{lager_console_backend, info},
   		
   	  
* Generate the Release Node, it will be located in j_cache/rel/jc

		[root@db01] ./rebar generate
        chmod a+x rel/jc/bin/jc
   
   or, for WINDOWS, 
   
       [root@db01] ./rebar generate -C rebar_windows.config


* Run the release

      [root@db01] ./bin/jc console` or `[root@dbo1] ./bin/jc attach



###Documentation
1.
        [root@dbo1] cd j_cache
    
        [root@dbo1] ./rebar doc skip_deps=true`


###Test
        [root@dbo1] cd j_cache
        [root@dbo1] ./rebar ct



###Performance

Performance charts can be found in the test/benchmark directory. Tests were done:

*  Using MacBook Pro, 2.5 GHz Intel Core i7, 16 GB 1600 Mhz DDR3
*  1 to 4 Nodes running j_cache,
*  basho_benchmark running on additional node utilizing jc_bridge 
*  10 concurrent processes randomaly assigned a j_cache node
*  gets, puts, and evicts at a 5:3:2 ratio.
*  100 Kilobyte-sized, binary values
