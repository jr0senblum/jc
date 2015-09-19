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
%%% 
%%% Server responds to CONNECT frames with BINARY STRINGS.
%%%
%%% VERSION:1.0
%%% or
%%% error and the socket will be closed.
%%%
%%% COMMAND frames consist of an 8 bytes size followed by the command.
%%%
%%% For example:
%%% [15jc:put(bed,1,1)
%%%
%%% Closing a CONNECTION is a COMMAND frame as follows
%%% CLOSE
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
			    command = undefined :: undefined |
						   version |
						   binary(),
			    size = undefined    :: undefined | 
						   non_neg_integer(),
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
					size = undefined,
					command = undefined},
			  ?TIMEOUT).

%% -----------------------------------------------------------------------------
%% @private Handle info messages: Socket messages and jc_psub subscription 
%% messages.
%%
-spec handle_info(any(),#jc_pro_state{}) -> {noreply,#jc_pro_state{}}.

handle_info({tcp, S, Data}, State = #jc_pro_state{socket=S, transport=T})->
    NewState = protocol(Data, State),
    T:setopts(S, [{active, once}]),
    {noreply, NewState, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State = #jc_pro_state{socket=S, transport = T}) ->
    T:send(S, marshal(Msg)),
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
	throw:{fatal, F} ->
	    T:send(S, marshal({error, {protocol, F}})),
	    self() ! {tcp_closed, S};
	_:E ->
	    T:send(S, marshal({error, E})),
	    reset_state(State)
    end.
	

%% Not CONNECTED, assume we are in a CONNECT frame, looking for VERSION
%% segment.
%%
proto(B, #jc_pro_state{connected=false, transport = T, socket = S} = State) ->
    NewState = connect(B, State),

    case NewState#jc_pro_state.connected of
	true ->
	    Version = NewState#jc_pro_state.version,
	    lager:debug("~p (~p): connected with version: ~p",
			[?MODULE, self(), {Version}]),
	    T:send(S, marshal({version, <<"1.0">>}));
	false ->
	    ok
    end,
    NewState;


%% CONNECTED, parsing a COMMAND frame. If the socket grabbed more than one 
%% command, keep the left-over portion and use it as the starting portion
%% when more date comes.
%
proto(B, #jc_pro_state{command = <<>>}=State) ->
    <<Size:8, C/binary>> = B,
    get_command(C, <<>>, State#jc_pro_state{size=Size});

proto(B, #jc_pro_state{command=Com}=State) ->
    get_command(B, Com, State).



% Walk the binary accumulating characters until we have grabbed the adveritesed
% amount. Execute the command, take care of any bytes left over.
%
get_command(B, Acc, #jc_pro_state{size=0} = State) ->
    parse(B, State#jc_pro_state{command = Acc});

get_command(<<>>, Acc, State) ->
    State#jc_pro_state{command=Acc};

get_command(<<C:1/binary, B/binary>>, Acc, #jc_pro_state{size=Size} = State) when Size > 0 ->
    get_command(B, <<Acc/binary, C/binary>>, State#jc_pro_state{size = Size-1}).


% Parse and execute the COMMAND.
parse(B, #jc_pro_state{transport = T, socket = S, command = Com} = State) ->
    lager:debug("~p (~p): executing command: ~p",  [?MODULE, self(), Com]),

    Edn = binary_to_list(Com),
    [M, F, A] = erldn:to_erlang(element(2, erldn:parse_str(Edn))),
    NewA = case F of
	       _ when F == map_subscribe;
		      F == map_unsubscribe;
		      F == topic_subscribe;
		      F == topic_unsubscribe ->
		   [self()| A];
	       _ ->
		   A
	   end,

    R = apply(M, F, NewA),
    T:send(S, marshal(R)),
    case B of
	<<>> ->
	    reset_state(State);
	_ ->
	    <<Size:8, B2/binary>> = B,
	    get_command(B2, <<>>, State#jc_pro_state{command = <<>>, size=Size})
    end.



% CONNECT FRAME parsing
% 
connect(<<"VERSION:1.0\r\n", _B/binary>>, S=#jc_pro_state{command=version}) ->
    reset_state(S);
connect(<<"CONNECT\r\n", B/binary>>, S) ->
    connect(B, S#jc_pro_state{command=version});
connect(<<>>, State) ->
    State;
connect(B, _State) ->
    throw({fatal, {bad_connect_frame, B}}).



% ==============================================================================
% Utility functions
% ==============================================================================


%% After a COMMAND frame, reset the CONNECTION (state) for next COMMAND frame.
reset_state(S) ->
    S#jc_pro_state{command = <<>>,
		   size = undefined,
		   connected=true}.


% Messsage -> [Size]Message
marshal(Message) ->
    Bin = iolist_to_binary(jc_edn:to_edn(Message)),
    Size = byte_size(Bin),
    <<Size:8, Bin/binary>>.


