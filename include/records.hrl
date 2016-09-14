% Types
-type seconds()          :: non_neg_integer().
-type ttl()              :: seconds().
-type time_stamp()       :: seconds().

-type map_name()         :: any().
-type key()              :: any().
-type value()            :: any().

-type rec_ref()          :: reference().



% Key_to_value - an ordered_set table whose key is {key, map_name}. Ref is used
% by jc_eviction manager as the key of the cache item to evict. i1 - i4 are 
% fields that can be used to hold values pulled from a json value to support an
% indexed querry-select feature (see README and jc_store:start_indexing/2 for 
% more).
-record (key_to_value,  
	 {jc_key         :: {key(), map_name()}, 
	  map            :: map_name(),
	  key            :: key(),
	  value          :: value(), 
	  i1             :: value(),
	  i2             :: value(),
          i3             :: value(),
          i4             :: value(),
	  create_tm      :: time_stamp(),
	  last_update    :: time_stamp(),
	  ttl_secs       :: ttl(), 
	  ref            :: rec_ref()
	 }).

-type key_to_value()     :: #key_to_value{}.


% Seq_no is an integer supplied by the client that, if provided, MUST be
% strictly monotinic and is used as a sequence number to ensure that a stale
% operation doesn't make it to jcache after the fact and clobber a more recent
% put or evict operation.
-record(seq,
	{map            :: map_name(),
	 seq_no         :: jc_sequence:seq()
	}).


% Defines the index for a given map and JSON path. Used for query-selects and
% evicts. Position indicates which column (i1-i4) in key_to_value to store the
% index.
-record (to_index,
	 {map_path      :: {map_name(), tuple()},
	  map_name      :: map_name(),
	  position      :: non_neg_integer()    
	 }).
	  

% Record that keeps track of the accounting around a JSON query as part of 
% determining whether an index should be initiatied.
-record (auto_index,
	 {map_path      :: {map_name(), tuple()} | '_',
	  count         :: non_neg_integer() | '_',
	  first         :: integer() | '_',
	  last          :: integer() | '_',
	  times         :: [integer()] | '_',
	  indexed       :: boolean() | '_',
	  success       :: boolean() | '_',
	  error_message :: term() | '_'
	 }).
	    

% Information about timers (erlang:send_after/3) used for item-level ttl. The 
% key to this table is the unique record referencece of the key_to_value item to
% evict. Used for record-level time to live.
-record (ttl,           
	 {key            :: rec_ref(),   % Unique Ref stored with {K, V}
	  timer_ref      :: reference(), % Timer Reference
	  ttl_secs       :: seconds()
	 }).


% Map-level ttl table.
-record (max_ttl,           
	 {map            :: map_name(),
	  ttl_secs       :: seconds()
	 }).


% In the future this could hold other stats, hit%, etc.
-record (stats,           
	 {key            :: atom(), 
	  value          :: any()
	 }).


% Jc_psub records. Subscription patterns and the set of PIDS subscribed to those
% patterns.
-record (ps_sub,
	 {subscription            :: term(),
	  clients = sets:new()    :: sets:set()
	 }).


% Ps_client records. Unique processes that are subscribers, includings what type
% of mechanism is used to monitor the client - link to the Pid or monitor the 
% node.
-record (ps_client,
	 {pid            :: pid(),
          m_type         :: {ref, reference()} | {link, any()}
	 }).
