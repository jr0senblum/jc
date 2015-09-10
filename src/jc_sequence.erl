%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%% This gen_server is a singleton serializing access to the sequence table by
%%% which the cache provides support for serialized operations.
%%% 
%%% When the gen_server starts it tries to register itself using Global as
%%% jc_sequence. If not successful, it monitors the PID currently associated
%%% with jc_cluster. If that PID goes down, this gen_server tries to assign
%%% itself the global name.
%%% @end
%%% Created : 25 Aug 2015 by Jim Rosenblum <jrosenblum@carelogistics.coml>
%%% ----------------------------------------------------------------------------
-module(jc_sequence).

-behaviour(gen_server).


%% module API 
-export([start_link/0,
	 test_set/2]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export_type ([seq/0]).

-type seq() :: non_neg_integer() | -1.

 % Table and record definitions.
-include("../include/records.hrl").   



-define(SERVER, ?MODULE).


-record(jc_seq_state, {}).


%%% ============================================================================
%%% Modue API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc Ask the server to do the test_set: If NewSeq is >= to the existing 
%% sequence number for the Map, set the sequence to NewSeq and return true, 
%% else false.
%%
-spec test_set(Map::map(), NewSeq::seq()) -> true | false.

test_set(Map, Seq) ->
    gen_server:call({global, ?MODULE}, {test_set, Map, Seq}, 1000).


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
%% @private Initialize the server by trying to register under the global name,
%% jc_sequence.
%%
-spec init([]) -> {ok, #jc_seq_state{}}.

init([]) ->
    lager:info("~p: up.", [?MODULE]),

    grab_name(),
    {ok, #jc_seq_state{}}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages
%%
-spec handle_call(term(), {pid(), _}, #jc_seq_state{}) -> 
			 {reply, ok, #jc_seq_state{}}.

handle_call({test_set, Map, Seq}, _From, State) ->
    Reply = do_test_set(Map, Seq),
    {reply, Reply, State};


handle_call(Request,  _From, State) ->
    lager:warning("~p: unexpected call request: ~p.",[?SERVER, Request]),
    {reply, ok, State}.



%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_seq_state{}) -> {noreply, #jc_seq_state{}}.

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.",[?SERVER, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Handle info messages: the master jc_sequence goes down, compete to
%% make this one master.
%%
-spec handle_info(any(), #jc_seq_state{}) -> {noreply, #jc_seq_state{}}.

handle_info({'DOWN', _MonitorRef, _Type, Object, Info}, State) ->
    lager:debug("~p: jc_sequence master at ~p went down with ~p.", 
		 [?SERVER, Object, Info]),
    grab_name(),
    {noreply, State};


handle_info(Msg, State) ->
    lager:warning("~p: unexpected info message: ~p.",[?SERVER, Msg]),
    {noreply, State}.



%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_seq_state{}) -> any().

terminate(_Reason, _State) ->
    ok.



%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_seq_state{}, any()) -> {ok, #jc_seq_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% If the global registry doesn't have a PID registered for jc_sequence, then 
%% claim it; otherwise, monitor the master.

grab_name() ->
    case global:register_name(?MODULE, self()) of
	yes ->
	    lager:info("~p: master is ~p.", [?MODULE, self()]);
	no ->
	    Master = global:whereis_name(?MODULE),
	    lager:info("~p: master is: ~p.", [?MODULE, Master]),
	_ = monitor(process, Master)
    end.


%% -----------------------------------------------------------------------------
%% Compare and set: if NewSeq >= existing sequence set sequence to NewSeq and
%% return true, else false.
%%

do_test_set(Map, NewSeq) ->
    case mnesia:dirty_read({seq, Map}) of
	[] -> 
	    mnesia:dirty_write(#seq{map = Map, seq_no = NewSeq}),
	    true;
	[#seq{seq_no = High}] when High =< NewSeq ->
	    mnesia:dirty_write(#seq{map = Map, seq_no = NewSeq}),
	    true;
	[#seq{seq_no = High}] when High > NewSeq ->
	    false
	% let it crash
        %_ ->	    false
    end.
