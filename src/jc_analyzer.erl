%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc This gen_server provides update_statistcs/2 that is called by {@link 
%%% jc_store. jc_store} whenever a json_query is used. Frequency of use is 
%%% tracked, and if the {Map, Json Query} is used sufficiently often, an index
%%% is created.
%%%
%%% @end
%%% Created : 27 Aug 2015 by Jim Rosenblum <jrosenblum@carelogistics.com>
%%% ----------------------------------------------------------------------------
-module(jc_analyzer).

-behaviour(gen_server).


%% Module API
-export([dump/0, 
	 start_link/0, 
	 update_statistic/2]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).


% Record and type definitions.
-include("../include/records.hrl").   

 % Use QLC for some querying.
-include_lib("stdlib/include/qlc.hrl").


-define(SERVER, ?MODULE).
-define(INTERVAL, (1000 * 60 * 60)). % 1 hour interval to do clean-up job.


% Start indexing if occurances of json query exceeds limit within window_sec.
-record(jc_analyzer_state, {limit      :: non_neg_integer(),
			    window_sec :: non_neg_integer()}).


%%% ============================================================================
%%% MODULE API
%%% ============================================================================



%% -----------------------------------------------------------------------------
%% Create or update the statistics entry which tracks ths use of the json query
%% on the given map. 
%%
-spec update_statistic(map(), tuple()) -> ok.

update_statistic(Map, Path) ->
    gen_server:cast({global, ?MODULE}, {update_stat, Map, Path}).


%% -----------------------------------------------------------------------------
%% Return a list of #auto_index rows being tracked.
%%.
-spec dump() -> [#auto_index{}] | {error, timeout}.

dump() ->
    gen_server:call({global, ?MODULE}, dump, 1000).


%% -----------------------------------------------------------------------------
%% @doc Start the server and link the caller to it.
%%
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private Initialize the server by getting its default configuration and 
%% trying to become the master, and, if successful, schedule the first clean 
%% job.
%%
-spec init([]) -> {ok, #jc_analyzer_state{}}.

init([]) ->
    {Cnt, Window} = application:get_env(jc, analyze_freq, {5, 120}),
  
    lager:info("~p: up with threshold of ~p times in ~p seconds.",
	       [?MODULE, Cnt, Window]),
    
    grab_name(),
    {ok, #jc_analyzer_state{limit = Cnt, window_sec = Window}}.
    


%% -----------------------------------------------------------------------------
%% @private Hande call messages. Dump the auto_index table.
%%
-spec handle_call(term(), {pid(), _}, #jc_analyzer_state{}) -> 
			 {reply, ok, #jc_analyzer_state{}}.

handle_call(dump, _From, State) ->
    Reply = mnesia:dirty_match_object(#auto_index{_ = '_'}),
    {reply, Reply, State};

handle_call(Request,  _From, State) ->
    lager:warning("~p: unexpected call request: ~p.",[?SERVER, Request]),
    {reply, ok, State}.


%% -----------------------------------------------------------------------------
%% @private Handle cast messages: Update statistics about a Map and Path.
%%
-spec handle_cast(any(), #jc_analyzer_state{}) -> 
			 {noreply, #jc_analyzer_state{}}.

handle_cast({update_stat, Map, Path}, State) ->
    update_stat(Map, Path, State),    
    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.",[?SERVER, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Handle info messages: Master down, try to become master; 
%% Clean: clean the table of orphans rows - a sinlge use of a query search will 
%% be deleted via periodic process.
%%
-spec handle_info(any(), #jc_analyzer_state{}) -> {noreply, #jc_analyzer_state{}}.

handle_info({'DOWN', _Mon, _Type, Obj, Info}, State) ->
    lager:debug("~p:  master down at ~p with ~p.", [?SERVER, Obj, Info]),
    grab_name(),
    {noreply, State};

handle_info(clean, State) ->
    clean(State#jc_analyzer_state.window_sec),
    erlang:send_after(?INTERVAL, jc_analyzer, clean),
    {noreply, State};
    
handle_info(Info, State) ->
    lager:warning("~p: unexpected info message: ~p.",[?SERVER, Info]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_analyzer_state{}) -> any().

terminate(_Reason, _State) ->
    ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_analyzer_state{}, any()) -> 
			 {ok, #jc_analyzer_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% internal functions
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% If the global registry doesn't have a PID registered for jc_analyzer, then 
%% claim it and schedule frst clean; otherwise, monitor the master.

grab_name() ->
    case global:register_name(?MODULE, self()) of
	yes ->
	    lager:info("~p: master is: ~p.", [?MODULE, self()]),
	    erlang:send_after(?INTERVAL, jc_analyzer, clean);
	no ->
	    Master = global:whereis_name(?MODULE),
	    lager:info("~p: master is: ~p.",  [?MODULE, Master]),
	    _ = monitor(process, Master),
	    ok
    end.


%% -----------------------------------------------------------------------------
%% Create or update the row which is gathering statistics on the use of JSON 
%% querries. If one is used and there isn't an index for it, then a row should
%% be created; otherwise updated. If it has been used sufficiently ask for an 
%% index.
%% 
-spec update_stat(map(), tuple(), #jc_analyzer_state{}) -> ok.

update_stat(Map, Path, #jc_analyzer_state{limit=Limit, window_sec=Window}) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    case mnesia:dirty_read(auto_index, {Map, Path}) of
	[] -> % no records exists, create it.
	    lager:debug("~p: initiating tracking of ~p.", 
			[?SERVER, {Map, Path}]),
	    mnesia:dirty_write(#auto_index{map_path = {Map, Path},
					   count = 1,
					   first = NowSecs,
					   last = NowSecs,
					   times = [NowSecs],
					   indexed = false,
					   success = undefined,
					   error_message = none});

        % we have met the threshold and aren't indexing.
	[#auto_index{count=C, first=F, indexed=Indexed}=R] when
	      (C + 1) >= Limit, Indexed == false ->

	    case (NowSecs - F) =< Window of
		false ->
                    % Occurances took longer than window to accumulate
		    slide_window(R, NowSecs, Map, Path, Window);
		true ->
		    initiate_index(R, NowSecs, Map, Path)
	    end;

        % Already being indexed or haven't met the theshold in the window.
	[R] ->
	    slide_window(R, NowSecs, Map, Path, Window)
    end.


% Ask jc_store to create an index, record the response and mark the row indexed.
initiate_index(#auto_index{count = C, times = T} = R, NowSecs, Map, Path) ->
    {Success, Msg} = 
	case jc_store:create_index(Map, Path) of
	    ok ->
		lager:debug("~p: initiating index on ~p.", 
			    [?SERVER, {Map, Path}]),
		{true, none};
	    {error, E} ->
		lager:warning("~p: not able to start index: ~p, ~p.", 
			      [?SERVER, {Map, Path}, E]),
		{false, E}
		    
	end,
    mnesia:dirty_write(R#auto_index{count = C + 1,
				    last = NowSecs,
				    times = lists:sort([NowSecs|T]),
				    indexed = true,
				    success = Success,
				    error_message = Msg}).


% Remove all times, T,  such that now - T > WindowSize. What's left is the new 
% count.
%
slide_window(#auto_index{times = T} = R, NowSecs, Map, Path, Window) ->
    lager:debug("~p: increment count and slidinf window on ~p.", 
		[?SERVER, {Map, Path}]),
    
    NewTimes = cull(lists:sort([NowSecs|T]), NowSecs, Window),
    
    case NewTimes of
	[] ->
	    mnesia:dirty_delete_object(R);
	NewTimes ->
	    mnesia:dirty_write(R#auto_index{first = hd(NewTimes),
					    last = NowSecs,
					    times = NewTimes,
					    count = length(NewTimes)})
    end.
    

% Remove all times, T,  until the gap between T and now is less than the
% window. 
%
cull([], _NowSecs, _Window) ->
    [];
cull([T|Ts], NowSecs, Window) when (NowSecs - T) >= Window ->
    cull(Ts, NowSecs, Window);
cull(Ts, _NowSecs, _Window) ->
    Ts.


% Delete anything from the table, not being indexed, whose (now - last) is > 
% then the time allowed (the Window). 
%%
clean(Window) ->
    lager:debug("~p: cleaning auto_index table.", [?MODULE]),
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    QH = qlc:q([R || R <- mnesia:table(auto_index),
		     R#auto_index.indexed == false,
		     (NowSecs - R#auto_index.last) >  Window]),

    Records = [mnesia:dirty_delete_object(R) || 
		  R <- mnesia:async_dirty(fun() -> qlc:e(QH) end)],

    lager:debug("~p: deleted ~p records from auto_index table.",
		[?MODULE, length(Records)]).
