%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%% This is the back-end, protocol gen_server that, utilizing ranch, provides a
%%% very simple binary protocol to interact with jc.
%%% Message format is [size]command. The size does not include [size]. 
%%% For example:
%%% [15jc:put(bed,1,1)
%%% Responses follow the same format:
%%% [6]{ok,1}
%%%
%%% @end
%%% Created : 25 Aug 2015 by Jim Rosenblum <jrosenblum@carelogistics.coml>
%%% ----------------------------------------------------------------------------

-module(jc_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).


%% module API 
-export([start_link/4]).


%% gen_server callbacks
-export([init/1, init/4,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, infinity).
-define(DIG(D), 
	D >= <<$0>> andalso 
	D=< <<$9>>).

-record(jc_protocol_state, {socket, 
			    transport, 
			    command=undefined, 
			    size}).



%%% ============================================================================
%%% Modue API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Start the gen_server which will handle the socket.
%%
-spec start_link(ranch:ref(), any(), jc_protocol, any()) -> {ok,pid()}. 

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% This function is never called. Its defined so that the gen_server contract
%% is fufilled.
%%
-spec init([]) -> {ok, undefined}.

init([]) -> {ok, undefined}.


%% -----------------------------------------------------------------------------
%% Set up the socket and convert the process into a gen_server.
%%
-spec init(ranch:ref(), any(), jc_protocol, [Port::integer()]) -> none().

init(Ref, Socket, Transport, _Opts = [Port]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    lager:debug("~p(~p): up and listening on ~p.",
	       [?MODULE, self(), Port]),

    gen_server:enter_loop(?MODULE, [],
			  #jc_protocol_state{socket=Socket, transport=Transport},
			  ?TIMEOUT).


%% -----------------------------------------------------------------------------
%% @private Handle info messages: Socket messages and jc_psub subscription 
%% messages.
%%
-spec handle_info(any(),#jc_protocol_state{}) -> {noreply,#jc_protocol_state{}}.

handle_info({tcp, S, Data}, State = #jc_protocol_state{socket=S,
						       transport=T,
						       command=C,
						       size=Size}) ->
    T:setopts(S, [{active, once}]),
    {NewC, NewSize} = collect(T, S, Data, C, Size),
    {noreply, State#jc_protocol_state{command = NewC, size = NewSize}, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State = #jc_protocol_state{socket=S, transport = T}) ->
    T:send(S, marshal(Msg)),
    {noreply, State, ?TIMEOUT}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages.
%%
-spec handle_call(term(), {pid(), _}, #jc_protocol_state{}) -> 
			                         {reply, ok, #jc_protocol_state{}}.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.



%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_protocol_state{}) -> {noreply, #jc_protocol_state{}}.

handle_cast(_Msg, State) ->
	{noreply, State}.



%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_protocol_state{}) -> any().

terminate(_Reason, _State) ->
	ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_protocol_state{}, any()) -> 
			                              {ok, #jc_protocol_state{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================

collect(T, S, <<"[", B/binary>>, undefined, _Size) ->
    try
	start_command(T, S, B, <<>>)
    catch
	_:E ->
	    T:send(S, marshal({error, E})),
	    {undefined, undefined}
    end;

collect(T, S, Binary, Acc, Size) ->
    try
	Stripped = binary:part(Binary, {0, byte_size(Binary) - 2}),
	parse(T, S, Stripped, Acc, Size)
    catch
	_:E ->
	    T:send(S, marshal({error, E})),
	    {undefined, undefined}
    end.


start_command(T, S, <<"]", B/binary>>, Acc) ->
    CmdSize = binary_to_integer(Acc),
    Stripped = binary:part(B, {0, byte_size(B) - 2}),
    parse(T, S, Stripped, <<>>, CmdSize);
	     
start_command(T, S, <<N:1/binary, B/binary>>, Acc) when ?DIG(N) ->
    start_command(T, S, B, <<Acc/binary, N/binary>>).



parse(Transport, Socket, _B, Acc, 0) ->
    {ok,Scanned,_} = erl_scan:string(binary_to_list(<<Acc/binary, ".">>)),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, R, _} = erl_eval:exprs(Parsed, []),
    
    Transport:send(Socket, marshal(R)),
    {undefined, undefined};

parse(_Transport, _Socket, <<>>, Acc, Size) ->
    {Acc, Size};

parse(Transport, Socket, <<C:1/binary, B/binary>>, Acc, Size) ->
    parse(Transport, Socket, B, <<Acc/binary, C/binary>>, Size-1).


marshal(Message) ->
    Bin  = binary:list_to_bin(io_lib:format("~p", [Message])),
    Size = size(Bin),
    ["[",integer_to_list(Size),"]", Bin].
