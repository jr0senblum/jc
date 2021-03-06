JC  
====
## Erlang, Distributable, In-Memory Cache

### Featuring: Pub/Sub, JSON-query, and mechanisms to support consistency without transactoins.


[![Build Status](https://travis-ci.org/jr0senblum/jc.svg?branch=master)](https://travis-ci.org/jr0senblum/jc)
[![hex.pm version](https://img.shields.io/hexpm/v/jc.svg)](https://hex.pm/packages/jc)


### Features
* Cache entries are Map, Key, Value, [TTL], [Sequence]
  * Maps represent a name-space for Keys - similar to the notion
    of 'bucket'
    in other caching systems
  * Maps, Keys and Values can be any Erlang term
  * TTL is time-to-live in seconds
* Consistency assist
  * Client-Side Sequence Number: An alternative API allows for a sequence-number
    parameter on the put/x, evict/x, match/x and remove/x operations. Operations
    whose sequence number is lower than the current, per-map max are disallowed
    thereby ensuring, for example, that an old delete that shows up after a newer update
    does not inappropriately evict the newer update.
  * Node of Responsibility: A key-specific node can be identified for destructive
    operations (put, evict, etc.) thereby preserving eventual consistency without transactions. 
      * jc_store:locus/2 takes a Key and a list of nodes and returns the node of 
    responsibility or the given key.
      * jc_bridge accepts a message {From, {locus, Key}} and returns the node of
      responsibility. Because jc\_bridge knows which nodes are available, the client
      is relieved from keeping track of up-nodes, which is necessary to
      caclulate the correct node of responsiblility. For example:
  
 ~~~~ Erlang
    {jc_bridge, Any_Cache_Node} ! {self(), {locus, Key}},
    NOR = receive
        {error, _} -> error
              Node -> Node
    after
        1000 -> error
    end,
    {jc_bridge, NOR} ! {self, {put, benchtest, 10203, "{\"Key\":\"Value\"}
 ~~~~  
    Because data is everywhere, a lookup will always find a key irrespective of 
    the node of responsibility. The way to best use this is to configure jc_sequence
    to not be a singleton and then use the Node of Responsibility feature to chose
    the node to do the inserts/deletes using the jc_s API.
    
* JSON query support
  * Query by JSON: When Values are JSON, evict_match/2,
    evict_all_match/1 and values_match/2 will search or evict
    keys whose JSON value, at a location specificed by a java-style, 
    dot-path string, equals the given value. That is,
    jc:values_match(bed, "id.type=3") would return all values, in the given
    map (bed), where that value was a JSON object, id, with a "type":3
    at its top-level.
  * Ad-hoc, index support: In order to support faster
    operations, (2-3 orders of magnitude), each map can have up to four,
    dot-path, strings configured (or added at run-time) for which jc will
    provide index support.
   * Auto index recognition - Frequently used JSON querries will be
     automatically detected and indexing initiated.
* User controlled eviction
  * Map-level TTL: A job runs at configured intervals and removes
  items whose create-date is older than a map-specific, configured 
  number of seconds.
  * Item-level TTL: PUTS can include a TTL which defines when the
  item should be evicted. Used for shorter TTLs, as an exception
  to a Map-level TTL, or when more precision is required than that
  offered by the Map-level TTL.
* Pub/Sub 
  * Clients can subscribe to Map events for a specific key or
  for any key,  and for write, delete or either operations
  * Clients can create and subscribe to arbitrary 'topics' and 
  broadcast arbitrary messages under those topic names
  * Clients can subscribe to node-up and node-down events 
* Fine-grained logging via Lager



### Cache Functions (jc)
* Create
  * put(Map, Key, Value, [TTLSecs]) -> {ok, Key} | {error, badarg}
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs]) -> {ok, CountOfSuccessfulPuts} |
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
  * flush(silent) -> ok, Does not send alerts to subscribers

* Predicates
  * contains_key(Map, Key) -> true | false.
  * map_exists(Map) -> true | false.

* Meta
  * cache_nodes() -> {{active, [Node1,... ]}, {configured, [Node1,... ]}}
  * cache_size() -> {size, [{TableName, RecordCnt, Words}],...}
  * map_size(Map) -> {records, Count}
  * maps() -> {maps, [Map1, Map2,...]}
  * up() -> {uptime, [{up_at, String},{now, String},
                      {up_time, {D, {H, M, S}}}]


### Consistency Support Functions (jc_s)
Identical to the Create and Evict family of functions of the jc module
(see above), except:

* An additional sequence parameter, which is expected to be a monotonically
  incresing integer (with respect to a given Map), used to disalow
  "out of sequence" operations
* Functions return {error, out_of_seq} if out of sequence operation is attempted
  * evict(Map, Key, Seq)
  * evict_all_match(Criteria = "Json.Path.Match=Value", Seq) 
  * evict_match(Map, Criteria = "Json.Path.Match=Value", Seq)
  * put(Map, Key, Value, [TTLSecs], Seq) 
  * put_all(Map, [{K,V},{K,V},...], [TTLSecs], Seq)
  * remove_items(Map, Keys, Seq)
* Meta functions
  * sequence() -> [{Map, HighestNnumber},... ]
  * sequence(Map) -> HightestNumber



### Eviction Manager Functions (jc_eviction_manager)
* set_max_ttl(Map, Secs) -> ok | {error, badarg}
* get_max_ttls() -> [{Map, Secs}, ...]


### Pub/Sub Functions (jc_psub)
* map_subscribe(Pid, Map, Key|any, write|delete|any) -> ok | {error, badarg}
* map_unsubscribe(Pid, Map, Key|any, write|delete|any) -> ok | {error, badarg}
  * client receives
  
    `{map_event, {Map, Key, delete}}`
    or
    `{map_event, {Map, key, write, Value}`
* topic_subscribe(Pid, Topic, Value) -> ok | {error, badarg}
* topic_unsubscribe(Pid, Topic, Value) -> ok | {error, badarg}
    * client receives: {topic_event, {Topic, Value}}
* topic_event(Topic, Value) -> ok
  * Broadcasts Value to all
  subscribers of Topic
* topic_subscribe(Pid, jc_node_events, any) -> ok 
  * subscribes the user to node up and node down events:
  
  `{jc_node_events, {nodedown, DownedNode, [ActiveNodes],[ConfiguredNodes]}}`
  
  `{jc_node_events, {nodeup, UppedNode, [ActiveNodes],[ConfiguredNodes]}}`


### Indexing Functions (jc_store)
  * start_indexing(Map, Path={bed,"menu.id"}) -> ok |
                                               {error, no_indexes_available} |
							       {error, Term}
  * stop_indexing(Map, Path={bed,"menu.id"}) -> ok
  * indexes(Map) -> {indexes, [{{Map, Path}, Position},...]} for all indexes
                                                  of given Map
  * indexes() -> {indexes, [{{Map, Path}, Position},...]} for all indexes


### Interoperability: Bridge (jc_bridge)
 * All functions from the jc, jc_s, jc_eviction_manager, jc_psub
 and jc_store are supported and are of the form:
 
   `{From, {Fn, P1, P2,...}}`
   
    for each paramater, as in 
    
    `jc_bridge ! {Pid, {put, Map, Key, Value}}`
    
* Additionally, 
  `{From, locus, Key}} -> node()` Calls jc_store:locus/2 with the list of active
  cache nodes and returns the node of record.


  {From, {node_topic_sub}} -> ok | {error, badarg}, 
  client will recieve:
   
   `{jc_node_events, {nodedown, DownedNode, [ActiveNodes],[ConfiguredNodes]}}`
   
    or

    `{jc_node_events, {nodeup, UppedNode, [ActiveNodes],[ConfiguredNodes]}}`

   `{From, {node_topic_unsub}} -> ok`.


### Configuration
* Application configuration is in sys.config which is heavily
  commented
* Cookie, node-name and auto-restart of VM controlled by vm.args


### Application Modules
* jc_cluster
  * Simple, mnesia-based, cluster creation and management
* jc, jc_s, jc_store, jc_eviction_manager
  * Caching operations, Key-specific and Map-level TTLs
* jc_sequence
  * [optionally] Singleton service enforcing strictly monotonic sequence 
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
* jc_netsplit
  * Looks for evidence of node dis/apperation and implements a recovery
    strategy

### Net Split/Join Strategy
Mnesia does not merge on its own when a node joins (returns) to a mesh of nodes.
There are two situations where this is relevant:

* j_cache nodes start in a disconnected state so more than one initiates a new
cluster and then, subsequently, those nodes join into one cluster;
* A node drops out of the cluster due to some network glitch and then rejoins.

To handle these situations, whenever a cluster is created by a Node (node@123.45.67, 
for example), it creates a ClusterId - its Node name (node@123.45.67), for that cluster.

Given this ClusterId, we have the following strategy:

1. _Cluster Creation_: creates an initial ClusterId;
2. _Nodedown_: If the Node that created the cluster dissapears, a surviving Node changes the
   ClusterId such that ClusterId is now this new Node's name. In the case of a
   disconnected newtwork, one of the islands will have the original ClusterId Node 
   dissapear, and it will create a new one as described.
3. _Nodeup_ Whenever a Node appears, an arbitary Node ensures that any Nodes that report
    a different ClusterId (different than the arbitrary Node's ClusterId) are killed to be
    restarted by the hearbeat application. If any Nodes required restarting, the entire 
    cache is flushed or not per policy in config.sys.

### Build Instructions
* Ensure that Erlang 17 or higher is installed
* Get the Source Code from Stash

   `[root@db01] git clone https://github.com/jr0senblum/jc.git`

 * Edit the sys.config and vm.args files in ./config
    * vm.args: Indicate the correct node names and cookie
    * sys.config: Adjust prarameters as neccesary.

     `[root@db01] ./rebar3 release`
    
     or

     `[root@db01] ./rebar3 as prod release`
   	

### Documentation

   `[root@dbo1] ./rebar3 edoc`

## Component and Sequence Diagrams

### Components


![](https://cloud.githubusercontent.com/assets/2043491/10463722/1aff2856-71b4-11e5-8e0a-5fcbee0c3ea3.png)

![](https://cloud.githubusercontent.com/assets/2043491/10463734/2446743c-71b4-11e5-8c6a-6de2da844fbf.png)

![](https://cloud.githubusercontent.com/assets/2043491/10463736/290403f4-71b4-11e5-8b94-bd7273d4c7fa.png)


### Sequence Diagrams
![](https://cloud.githubusercontent.com/assets/2043491/10463819/9c59b7f4-71b4-11e5-9db9-a82fa762240c.png)

