% Types
-type map_name()         :: atom().
-type key()              :: any().
-type value()            :: any().

-type rec_ref()          :: reference().

-type seconds()          :: non_neg_integer().
-type ttl()              :: seconds().
-type time_stamp()       :: seconds().
-type seq()              :: non_neg_integer() | -1.
-type create_tm()        :: time_stamp().
-type last_update_tm()   :: time_stamp().



% Key_to_value - an ordered_set table whose key is {key, map}. Ref is used by
% jc_eviction manager as the key of the cache item to evict. i1 - i4 are 
% fields that can be used to hold values pulled from a json value to support a
% querry-select feature (see README and jc_store:start_indexing/2 for more. Seq
% is an integer supplied by the client that, if provided, is expected to be
% strictly monotinic. If it is not, the put with the non monotonic value will
% be evicted and the old one re-inserted.
%
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


-record(seq,
	{map            :: map_name(),
	 seq_no         :: seq()
	}).

% Holds information about json-path's that will be the target of a query-sellect
% for a given map. Position indicates which column (i1-i4) in key_to_value to 
% use.
-record (to_index,
	 {map_path      :: {map_name(), tuple()},
	  map_name      :: map_name(),
	  position      :: non_neg_integer()    
	 }).
	  

-record (auto_index,
	 {map_path      :: {map_name(), tuple()},
	  count         :: non_neg_integer(),
	  first         :: integer(),
	  last          :: integer(),
	  times         :: [integer()],
	  indexed       :: boolean(),
	  success       :: boolean(),
	  error_message :: term()
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
