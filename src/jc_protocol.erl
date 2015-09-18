%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%% This is the back-end, protocol gen_server that, utilizing ranch, provides a
%%% very simple BINARY STRING protocol to interact with jc. All of the lines 
%%% shown below are assumed to be BINARY STRINGS.
%%%
%%% The CONNECT frame:
%%% CONNECT\r\n
%%% VERSION:1.0\r\n
%%% CRLF:TRUE|FALSE\r\n
%%% USERNAME:username\r\n
%%% PASSWORD:password\r\n
%%% 
%%% CRLF MUST be TRUE if COMMAND frames terminate with \r\n. Otherwise,
%%% Otherwise, CRLF MUST be FALSE and lines MUST NOT be terminated with \r\n. 
%%% Server responds to CONNECT frames with BINARY STRINGS.
%%%
%%% [11]VERSION:1.0
%%% or
%%% [5]error and the socket will be closed.
%%%
%%% COMMAND frames consist of the byte_size enclosed in brackets
%%% followed by the command. If CRLF was set to TRUE when connecting than
%%% COMMAND lines MUST be terminated with \r\n; otherwise they MUST NOT.
%%% The supplied byte_size does not include the [byte_size] portion of the frame
%%% nor does it include any terminating \r\n characters.
%%%
%%% For example:
%%% [15jc:put(bed,1,1)
%%%
%%% Closing a CONNECTION is a COMMAND frame as follows
%%% [5]CLOSE
%%%
%%% RESPONSE frames are identical to COMMAND frames, but NEVER end with \r\n.
%%% Like all other lines, RESPONSE lines are BINARY STRINGS
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

-record(jc_pro_state, {socket, 
			    transport           :: 'jc_protocol', 
			    connected = false   :: boolean(),
			    have_size = false   :: boolean(),
			    command = undefined :: term(),
			    size = undefined    :: undefined | 
						   non_neg_integer(),
			    crlf = undefined    :: boolean() | undefined,
			    username = <<"">>   :: binary(),
			    password = <<"">>   :: binary(),
			    version = <<"1.0">> :: binary()}).



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

init(Ref, S, T, _Opts = [Port]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = T:setopts(S, [{active, once}]),
    lager:debug("~p(~p): up and listening on ~p.",
	       [?MODULE, self(), Port]),

    gen_server:enter_loop(?MODULE, [],
			  #jc_pro_state{socket = S, 
					     transport = T,
					     connected = false,
					     have_size = false,					     
					     size = undefined,
					     command = undefined},
			  ?TIMEOUT).

%% -----------------------------------------------------------------------------
%% @private Handle info messages: Socket messages and jc_psub subscription 
%% messages.
%%
-spec handle_info(any(),#jc_pro_state{}) -> {noreply,#jc_pro_state{}}.

handle_info({tcp, S, Data}, State = #jc_pro_state{socket=S, transport=T})->
    T:setopts(S, [{active, once}]),
    NewState = protocol(Data, State),
    {noreply, NewState, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State = #jc_pro_state{socket=S, transport = T}) ->
    T:send(S, jc_edn:to_edn(Msg)),
    {noreply, State, ?TIMEOUT}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages.
%%
-spec handle_call(term(), {pid(), _}, #jc_pro_state{}) -> 
			                         {reply, ok, #jc_pro_state{}}.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.



%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_pro_state{}) -> {noreply, #jc_pro_state{}}.

handle_cast(_Msg, State) ->
	{noreply, State}.



%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_pro_state{}) -> any().

terminate(_Reason, _State) ->
	ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_pro_state{}, any()) -> 
			                              {ok, #jc_pro_state{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% =============================================================================
%% Top-level protcol parser
%%
protocol(B, #jc_pro_state{transport = T, socket = S} = State) ->
    try
	proto(B, State)
    catch
	throw:{fatal, _F} ->
	    T:send(S, marshal(error)),
	    self() ! {tcp_closed, S};
	_:_ ->
	    T:send(S, marshal(error)),
	    reset_state(State)
    end.
	

%% Not CONNECTED, assume we are in a CONNECT frame, looking for VERSION, CRLF
%% USERNAME and PASSWORD segments.
%%
proto(B, #jc_pro_state{command=C,connected=false,transport=T,socket=S}=State) ->
    NewState = case C of 
		   X when X == undefined; X == version; X == crlf ->
		       connect(B, State);
		   username ->
		       username(B, State);
		   password ->
		       password(B, State)
	       end,
    case NewState#jc_pro_state.connected of
	true ->
	    Version = NewState#jc_pro_state.version,
	    lager:debug("~p (~p): connected ~p",
			[?MODULE, self(), {Version,
					   #jc_pro_state.username}]),
	    T:send(S, <<"[11]VERSION:1.0">>);
	false ->
	    ok
    end,
    NewState;

%% CONNECTED, parsing a COMMAND frame - look for the size of the COMMAND.
proto(B,#jc_pro_state{connected=true,have_size=false,crlf=F,size=Size}=State) ->
    Stripped = F(B),
    NewState = case Size of
		   undefined ->
		       get_size(Stripped, <<>>, State);
		   Partial ->
		       get_size(Stripped, Partial, State)
	       end,
    NewState;

%% Parsing COMMAND frame, size has been retrieved, accumulate the command.
proto(B,#jc_pro_state{connected=true,have_size=true,command=Com,crlf=F}=State) ->
    Stripped = F(B),
    get_command(Stripped, Com, State).



%% Walk the binary, accumulating the size from [N] portion of the COMMAND frame.
get_size(<<>>, Acc, State) ->
    State#jc_pro_state{size=Acc};

get_size(<<"[", B/binary>>, Acc, State) ->
    get_size(B, Acc, State);

get_size(<<N:1/binary, B/binary>>, Acc, State) when ?DIG(N) ->
    get_size(B, <<Acc/binary, N/binary>>, State);

get_size(<<"]", B/binary>>, Acc, State) ->
    CmdSize = binary_to_integer(Acc),
    get_command(B, <<>>, State#jc_pro_state{size=CmdSize, have_size = true});
get_size(_B, _A, _State) ->
    throw({command, bad_size_segment}).



% Walk the binary accumulating Size characters from the COMMAND frame, when no 
% more characters (size = 0), execute the command.
%
get_command(_B, Acc, #jc_pro_state{size=0} = State) ->
    parse(State#jc_pro_state{command = Acc});

get_command(<<>>, Acc, State) ->
    State#jc_pro_state{command=Acc};

get_command(<<C:1/binary, B/binary>>, Acc, #jc_pro_state{size=Size} = State) when Size > 0 ->
    get_command(B, <<Acc/binary, C/binary>>, State#jc_pro_state{size = Size-1}).


% Execute the COMMAND.
parse(#jc_pro_state{socket = S, command = Com} = State) ->
    lager:debug("~p (~p): executing command: ~p",  
		[?MODULE, self(), Com]),

    Edn = binary_to_list(Com),
    Payload = list_to_tuple(erldn:to_erlang(element(2, erldn:parse_str(Edn)))),

    case Payload of
	{close} ->
	    self() ! {tcp_closed, S};
	_ ->
	    jc_bridge ! {self(), Payload},
	    reset_state(State)
   end.



% CONNECT FRAME parsing
% 
connect(<<"CONNECT\r\n", B/binary>>, S=#jc_pro_state{command = undefined}) ->
    connect(B, S#jc_pro_state{command=version});
connect(<<"VERSION:1.0\r\n", B/binary>>, S=#jc_pro_state{command=version}) ->
    connect(B, S#jc_pro_state{version = <<"1.0">>, command=crlf});
connect(<<"CRLF:TRUE\r\n", B/binary>>, S=#jc_pro_state{command=crlf}) ->
    connect(B, S#jc_pro_state{command=username, crlf=fun(I) -> crlf(I) end});
connect(<<"CRLF:FALSE\r\n", B/binary>>, S=#jc_pro_state{command=crlf}) ->
    username(B, S#jc_pro_state{command=username, crlf=fun(I) -> I end});
connect(<<>>, State) ->
    State;
connect(B, _State) ->
    throw({fatal, {bad_connect_frame, B}}).


username(<<"USERNAME:", B/binary>>, S) ->
    username(B, S);
username(<<"\r\n", B/binary>>, S)->
    password(B, S#jc_pro_state{command = password});
username(<<C:1/binary, B/binary>>, #jc_pro_state{username = U} = S)-> 
    username(B, S#jc_pro_state{username = <<U/binary, C:1/binary>>});
username(<<>>, S) ->
    S.


password(<<"PASSWORD:", B/binary>>, S) ->
    password(B, S);
password(<<"\r\n", _B/binary>>, S)->
     S#jc_pro_state{connected = true};
password(<<C:1/binary,B/binary>>, #jc_pro_state{password = P} = S)-> 
    password(B, S#jc_pro_state{password = <<P/binary, C:1/binary>>});
password(<<>>, State) ->
    State.



% ==============================================================================
% Utility functions
% ==============================================================================


%% Strip off [carriage-return]lf
crlf(<<"\r\n">>) ->
    <<>>;
crlf(<<"\n">>) ->
    <<>>;
crlf(<<>>) ->
    <<>>;
crlf(B) when erlang:binary_part(B, byte_size(B) - 2, 2) == <<"\r\n">> ->
    binary:part(B, {0, byte_size(B) - 2});

crlf(B) when erlang:binary_part(B, byte_size(B) - 1, 1) == <<"\n">> ->
    binary:part(B, {0, byte_size(B) - 1});
crlf(B) ->
    B.



%% After a COMMAND frame, reset the CONNECTION (state) for next COMMAND frame.
reset_state(S) ->
    S#jc_pro_state{have_size = false,
			command = undefined,
			size = undefined}.


% Messsage -> [Size]Message
marshal(Message) ->
    Bin  = binary:list_to_bin(io_lib:format("~p", [Message])),
    Size = byte_size(Bin),
    ["[",integer_to_list(Size),"]", Bin].

