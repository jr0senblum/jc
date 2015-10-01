%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2015 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%%-------------------------------------------------------------------
-module(t).

-behaviour(gen_server).

%% API
-export([start_link/0, send/1, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, left, client}).

%%%===================================================================
%%% API
%%%===================================================================

reset()->
    gen_server:cast(?SERVER, reset).

send(Message) ->
    M  = case is_binary(Message) of 
	     true -> Message;
	     false -> list_to_binary(Message)
	 end,
    Size = byte_size(M),
    gen_server:call(?SERVER, {out, self(), <<Size:8, M/binary>>}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Pid]) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1},5555, [{active, true}, binary]),
    C = <<"{connect,{version,\"1.0\"}}">>,
    Size = byte_size(C),
    M = <<Size:8, C/binary>>,
    gen_tcp:send(Socket, M),
    {ok, #state{socket=Socket, left = <<>>, client=Pid}}.

%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
handle_call({out, Pid, Message}, _From, #state{socket=S}=State) ->
    gen_tcp:send(S, Message),
    {reply, ok, State#state{client=Pid}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(reset, #state{socket=S}=State) ->
    gen_tcp:close(S),


    {ok, Socket} = gen_tcp:connect({127,0,0,1},5555, [{active, true}, binary]),
    C = <<"{connect,{version, \"1.0\"}}">>,
    Size = byte_size(C),
    M = <<Size:8, C/binary>>,
    gen_tcp:send(Socket, M),
    {noreply, State#state{socket=Socket, left = <<>>}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Port, Message}, #state{socket=S, left=L, client=C}=State) ->
    L2 = get_command(C, <<L/binary, Message/binary>>),
    
    inet:setopts(S, [{active, once}]),
    {noreply, State#state{left=L2}};


handle_info(_Info, State) ->
    {noreply, State}.

get_command(C, Message) ->
    <<Size:8, M/binary>> = Message,

    case byte_size(M) of
	Size ->
	    C ! M,
	    <<>>;
	L when L > Size ->
	    <<M2:Size/bytes, B/binary>> = M,
	    io:format("=> ~s~n", [M2]),
	    get_command(C, B);
	L when L < Size ->
	    Message
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



    


