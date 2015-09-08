JC
====

##Erlang, Distributable, In-Memory Cache with Pub/Sub,  Serialization Assist and
JSON-Query Support.


###Features
* Cache entries are Map, Key, Value, [TTL], [Sequence]
  * Maps represent a name-space for Keys - similar to the notion
    of 'bucket'
    in other caching systems
  * {Map, Key} must be unique	
  * Maps, Keys and Values can be any Erlang term
  * TTL is time-to-live in seconds
* Consistency Assist through Serialization: An alternative API
    allows for a sequence-number parameter on the put/x, evict/x,
    match/x and remove/x operations. Operations whose sequence
    number is lower than the current, per-map max are disallowed 
    thereby ensuring, for example, that stale puts do not 
    overwrite "fresh" ones because the "fresh" one beat the stale
    one to jc.
 *  JSON Query Support
     * Query by JSON: When Values are JSON, evict_match/2,
       evict_all_match/1 and values_match/2 can search or evict
       keys whose values match a java-style, dot-path, string 
       (i.e., "id.type=3")
    * Ad-hoc, Index Support: In order to support faster
      operations, (2-3 orders of magnitude), each map can have up to four,
       dot-path, strings configured for which jc will provide 
       index support.
    * Auto Index Recognition - Ability to detect frequently used
      JSON querries and automatically start indexing on them.
* User Controlled Eviction
  * Map-level TTL: A job runs at configured intervals and removes
  items whose create-date is older than a map-specific, configured 
  number of seconds.
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
* Create
  * put(Map, Key, Value, [TTLSecs]) -> {ok, {key, Key}} | {error, badarg}
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs]) -> {ok, CntSuccessfulPuts} |
                                                  {error, badarg}
* Delete
  * evict(Map, Key) -> ok
  * evict_all_match(Criteria = "Json.Path.Match=Value") -> ok
  * evict_map_since(Map, Age_In_Secs) -> ok
  * evict_match(Map, Criteria = "Json.Path.Match=Value") -> ok
  * remove_items(Map, Keys) -> {ok, [{K, V}, ...]} for each {K, V} deleted.
* Retrieve
  * get(Map, Key) -> {ok, Value} | miss
  * get_all(Map, [K1, K2, ...]) -> {ok, {Found=[{K1,V1},...], Misses=[K2,...]}}
  * key_set(Map) -> {ok, [K1, K2, ...]} for each Key in the Map
  * values(Map) -> {ok, [V1, V2, ...]} for each Value in the Map
  * values_match(Map, Criteria="JSon.Path.Match=Value") ->
                                                  {ok, [{K1,V1}, {K2,V2}, ...]}
* Flush
  * clear(Map) -> ok
  * flush() -> ok
  * flush(silent) -> ok <-- Does not send alerts to subscribers
* Predicates
  * contains_key(Map, Key) -> true | false.
* Meta
  * cache_nodes() -> {nodes, {active, [Node1,... ]}, 
                             {configured, [Node1,... ]}}
  * cache_size() -> {size, [{TableName, RecordCnt, Words}],...}
  * map_size(Map) -> {records, Count}
  * maps() -> [Map1, Map2,...]
  * up() -> {uptime, [{up_at, String},{now, String},
                      {up_time {D, {H, M, S}}}]


### Searializable Cache Functions (jc_s)
Identical to the standard, CRUD functions above, except that
* Additional sequence parameter which is expected to be a monotonically
  increcing integer which is used to disalow "out of sequence" operations
* Functions return {error, out_of_seq} if one attemts an out of sequence 
  operation
  * evict(Map, Key, Seq)
  * evict_all_match(Criteria = "Json.Path.Match=Value", Seq) 
  * evict_match(Map, Criteria = "Json.Path.Match=Value", Seq)
  * put(Map, Key, Value, [TTLSecs], Seq) 
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs], Seq)
  * remove_items(Map, Keys, Seq)
* Meta Functions
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
   
    for each paramater, as in 
    
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
  * Predefined, *jc_node_events* topic provides subscribers
  node-up and node-down notifications
* jc_bridge
  * Server that acts as a proxy between an external process and
  jc functionality



###Configuration
* Application configuration is in sys.config which is heavily
  commented
* Cookie, node-name and auto-restart of VM controlled by vm.args


###Build Instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash

   `[root@db01] git clone https://github.com/jr0senblum/jc.git`

 * Edit the sys.config and vm.args files in ./config
    * vm.args: Indicate the correct node names and cookie in
      vm.args
   * sys.config: Adjust prarameters as neccesary.

     `[root@db01] ./rebar3 release`
    
     or

     `[root@db01] ./rebar3 prod release`


   	

###Documentation

   `[root@dbo1] ./rebar3 edoc`

