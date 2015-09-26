JC  
====

##Erlang, Distributable, In-Memory Cache

### Pub/Sub; JSON-Query; Consistency Assist; and Simple, JSON over TCP, Binary Protocol for Interoperability.


[![Build Status](https://travis-ci.org/jr0senblum/jc.svg)](https://travis-ci.org/jr0senblum/jc)

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
    overwrite newer ones due to the newer one beating the stale
    ones to the cache.
*  JSON Query Support
  * Query by JSON: When Values are JSON, evict_match/2,
    evict_all_match/1 and values_match/2 search or evict
    keys whose JSON value, at a location specificed by a java-style, dot-path
    string, equals the given value. That is,
    jc:values_match(bed, "id.type=3") would return all values for Keys in the
    bed 'Map' whose JSON value was an object with an "id":3 in the top-level.
  * Ad-hoc, Index Support: In order to support faster
    operations, (2-3 orders of magnitude), each map can have up to four,
    dot-path, strings configured for which jc will provide 
    index support.
  * Auto Index Recognition - Frequently used JSON querries will be automatically
    detected and indexing initiated.
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
  * Clients can subscribe to node-up and node-down events 
* Interopability: JSON, binary protocol over TCP
  * JSON strings can be used over TCP to interoperate with the cache eco-system.
    UTF-8 JSON strings -> binary -> TCP --- TCP <- binary <- UTF-8 JSON strings  
* Bridge process that accepts messages from a client indicating
  cache operations, executes the cache operations and returns the
  results to the client. This has been used with JInterface to 
  interoperate with CLOJURE and Java clients
* Fine-grained logging via lager



###Cache Functions (jc)
* Create
  * put(Map, Key, Value, [TTLSecs]) -> {ok, {key, Key}} | {error, badarg}
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs]) -> {ok, {cnt, CntSuccessfulPuts}} |
                                                  {error, badarg}
* Delete
  * evict(Map, Key) -> ok
  * evict_all_match(Criteria = "Json.Path.Match=Value") -> ok
  * evict_map_since(Map, Age_In_Secs) -> ok
  * evict_match(Map, Criteria = "Json.Path.Match=Value") -> ok
  * remove_items(Map, Keys) -> {ok, [{K, V}, ...]} for each {K, V} deleted.
* Retrieve
  * get(Map, Key) -> {ok, {value, Value}} | miss
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
  * cache_size() -> {sizes, [{TableName, RecordCnt, Words}],...}
  * map_size(Map) -> {records, Count}
  * maps() -> {maps, [Map1, Map2,...]}
  * up() -> {uptime, [{up_at, String},{now, String},
                      {up_time, {D, {H, M, S}}}]


### Consistacny Supported Functions (jc_s)
Identical to the Create and Evict family of functions above, except:
* An additional sequence parameter, which is expected to be a monotonically
  incresing integer (with respect to a given Map), is used to disalow
  "out of sequence" operations
* Functions return {error, out_of_seq} if out of sequence operation is attempted
  * evict(Map, Key, Seq)
  * evict_all_match(Criteria = "Json.Path.Match=Value", Seq) 
  * evict_match(Map, Criteria = "Json.Path.Match=Value", Seq)
  * put(Map, Key, Value, [TTLSecs], Seq) 
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs], Seq)
  * remove_items(Map, Keys, Seq)
* Meta Functions
  * sequence() -> {sequences, [{Map, Highest_Number},... ]}
  * sequence(Map) -> {sequence, Hightest_Number}



###Eviction Manager Functions (jc_eviction_manager)
* set_max_ttl(Map, Secs) -> ok | {error, badarg}
* get_max_ttls() -> {ttls, [{Map, Secs}, ...]}


###Pub/Sub Functions (jc_psub)
* map_subscribe(Pid, Map, Key|any, write|delete|any) -> ok |
                                                   {error, badarg}
  * client will receive
    * {map_event, {Map, Key, delete}}, or
    * {map_event, {Map, key, write, Value}
* map_unsubscribe(Pid, Map, Key|any, write|delete|any) -> ok |
                                                   {error, badarg}
* topic_subscribe(Pid, Topic, Value) -> ok | {error, badarg}
  * client will receive {topic_event, {Topic, Value}}
* topic_unsubscribe(Pid, Topic, Value) -> ok | {error, badarg}
* topic_event(Topic, Value) -> ok. <-- Broadcasts Value to all
  subscribers of Topic
* topic_subscribe(Pid, jc_node_events, any) -> ok subscribtes the user
  to node up and node down events:
  
  `{jc_node_events, {nodedown, DownedNode, [ActiveNodes],[ConfiguredNodes]}}`
  
  `{jc_node_events, {nodeup, UppedNode, [ActiveNodes],[ConfiguredNodes]}}`


###Indexing Functions (jc_store)
  * start_indexing(Map, Path={bed,"menu.id"}) -> ok |
                                               {error, no_indexes_available} |
							       {error, Term}
  * stop_indexing(Map, Path={bed,"menu","id"}) -> ok
  * indexes(Map) -> {indexes, [{{Map, Path}, Position},...]} for all indexes
                                                  of given Map
  * indexes() -> {indexes, [{{Map, Path}, Position},...]} for all indexes


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


### Interoperability: JSON-based protocol
The protocol utilizes UTF-8 JSON strings binary encoded over TCP.

The protocol defines two message types CONNECT and COMMAND which are 
binary strings consisting of an 8-byte, size prefix followed by the 
CONNECT or COMMAND details.

Responses are also binary strings with an 8-byte, size prefix.

The CONNECT command initiates a session, 

    M = <<"{\"connect\": \"version\": \"1.0\"}">>

Size is 29, so the CONNECT message is:

    <<29:8, M/binary>> =
    <<29,123,34,99,111,110,110,101,99,116,34,58,123,34,118,
      101,114,115,105,111,110,34,58,34,49,46,48,34,125,125>>

The server will respond to a CONNECT command with either an error or
the encoded version of 

    M = "{\"version\": \"1.0\"}"
 
    <<17:8, M/binary>> = 
    <<17,123,34,118,101,114,115,105,111,110,34,58,34,49,46,48,34,125>>
 
COMMAND messages consist of an 8-byte prefix followed by the command.

The form of a COMMAND message is as follows
    {"m": Module, "f": Function, "a": [Arg1, Arg2, ...]}

A client session might look as follows (remove escape characters for legibility)

   client:send({"connect": "version": "1.0"})
    ==> {"version": :1.0"}

   client:send({"m":"jc", "f":"put", "a":["aMap", "aKey", "a string value"]})
    ==> {"status":"ok","key":"aKey"}>>

    client:send(({"m":"jc","f":"get","a":["aMap", "aKey"]}).
    ==> {"status":"ok","value":"a string value"}

The JSON commands map directly to cache Module Functions with the exception of the 
jc_psub subscription functions which do NOT need a self() parameter since
the per-session, TCP listener is the process which subscribes. So:

    client:send({"m":"jc_psub", "f":"map_subscribe", "a": ["evs" "any", "any"))").
    ===> ok

###Configuration
* Application configuration is in sys.config which is heavily
  commented
* Cookie, node-name and auto-restart of VM controlled by vm.args


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
* jc_protocol, jc_edn
  * Erlang -> Edn and protocol modules
* jc_psub: 
  * Pub / Sub of cache write and delete events
  * On-demand, ad-hoc topic events
  * Predefined, *jc_node_events* topic provides subscribers
  node-up and node-down notifications
* jc_bridge
  * Server that acts as a proxy between an external process and
  jc functionality


###Build Instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash

   `[root@db01] git clone https://github.com/jr0senblum/jc.git`

 * Edit the sys.config and vm.args files in ./config
    * vm.args: Indicate the correct node names and cookie
    * sys.config: Adjust prarameters as neccesary.

     `[root@db01] ./rebar3 release`
    
     or

     `[root@db01] ./rebar3 prod release`
   	

###Documentation

   `[root@dbo1] ./rebar3 edoc`
