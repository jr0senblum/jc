%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2015 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(t).

-behaviour(gen_server).

%% API
-export([start_link/0, send/1, reset/0, get_result/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, client, size, acc}).

%%% ============================================================================
%%% API
%%% ============================================================================

reset()->
    gen_server:cast(?SERVER, reset).

send(Message) ->
    M  = case is_binary(Message) of 
	     true -> Message;
	     false -> list_to_binary(Message)
	 end,
    Size = byte_size(M),
    gen_server:cast(?SERVER, {out, self(), <<Size:8/integer-unit:8, M/binary>>}).


%% -----------------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%% -----------------------------------------------------------------------------
init([Pid]) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1},5555, [{active, true}, binary]),
    C = <<"{connect,{version,\"1.0\"}}">>,
    Size = byte_size(C),
    M = <<Size:8/integer-unit:8, C/binary>>,
    gen_tcp:send(Socket, M),
    {ok, #state{socket=Socket, acc = <<>>, client=Pid, size = undefined}}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%% -----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%% -----------------------------------------------------------------------------
handle_cast({out, Pid, Message}, #state{socket=S}=State) ->
    gen_tcp:send(S, Message),
    {noreply, State#state{client=Pid}};

handle_cast(reset, #state{socket=S}=State) ->
    gen_tcp:close(S),

    {ok, Socket} = gen_tcp:connect({127,0,0,1},5555, [{active, once}, {packet,0},binary]),
    C = <<"{connect,{version, \"1.0\"}}">>,
    Size = byte_size(C),
    M = <<Size:8/integer-unit:8, C/binary>>,
    gen_tcp:send(Socket, M),
    {noreply, State#state{socket=Socket, acc = <<>>, size=undefined}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%% -----------------------------------------------------------------------------
handle_info({tcp,_P, Message}, #state{socket=S} = State) ->
    inet:setopts(S, [{active, once}]),
    NewState = handle_data(Message, State),
    {noreply, NewState};


handle_info(Info, State=#state{client=Client}) ->
    io:format("<== ~p~n", [Info]),
    Client ! Info,
    {noreply, State}.


handle_data(Data, State) ->
    do_command(Data,State).


do_command(<<>>, State) ->
    State;

do_command(<<Size:8/integer-unit:8, C/binary>>, #state{size = undefined, acc = <<>>, client=Client} = S) ->
    case byte_size(C) of
	CSize when CSize >= Size ->
	    <<Command:Size/binary, Remainder/binary>> = C,
	    io:format("==> ~s~n",[Command]),
	    Client ! Command,
	    do_command(Remainder, S);
	Less ->
	    S#state{size = Size - Less, acc = C}
    end;

do_command(Bin, #state{size = undefined, acc = Acc} = S) ->
    NewBin = <<Acc/binary, Bin/binary>>,
    case byte_size(NewBin) of
	NSize when NSize >= 8 ->
	    <<Size:8/integer-unit:8, Remainder/binary>> = NewBin,
	    do_command(Remainder, S#state{size=Size, acc = <<>>});
	_ ->
	    S#state{acc = NewBin, size = undefined}
    end;

do_command(Bin, #state{acc = Acc, size = Size, client=Client} = S) when size /= undefined->
    case byte_size(Bin) of 
	BSize when BSize >= Size ->
	    <<Seg:Size/binary, Remainder/binary>> = Bin,
	    Command = <<Acc/binary, Seg/binary>>,
	    io:format("==> ~s~n",[Command]),
	    Client ! Command,
	    do_command(Remainder, S#state{acc = <<>>, size = undefined});
	Less ->
	    S#state{size = Size - Less, acc = <<Acc/binary, Bin/binary>>}
    end.
    

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%% -----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%% -----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal functions
%%% ============================================================================
get_result() ->
    receive
	X -> X
    after 
	400 ->
	    error
    end.
