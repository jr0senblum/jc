%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%%
%%% JC Protocol 1.0
%%% A binary-encoded, string protocol used to provide socket-based 
%%% interoperability with JC. Incoming messages are string representations
%%% of a tuple. Responses are JSON.
%%%
%%% The protocol defines three message types: CONNECT, CLOSE and COMMAND all of
%%% which are binary strings consisting of a header, indicating the size of the
%%% message in bytes, follwed by the actual message itself. The header is an
%%% 8-byte, big endian, unsigned integer indicating the size of the message in 
%%% bytes.
%%%
%%% A RESPONSE is structured the same as messages - 8-byte size header followd
%%% by the content of the response.
%%%
%%% The CONNECT command initiates a session, 
%%%
%%% ```M = <<"{connect,{version,\"1.0\"}}">>''' 
%%%
%%% The byte size is 26, so the CONNECT message is:
%%%
%%% ```<<26:8/integer-unit:8, M/binary>> = 
%%%    <<0,0,0,0,0,0,0,26,123,99,111,110,110,101,99,116,44,123,118,101,114,
%%%      115,105,111,110,44,32,34,49,46,48,34,125,125>> '''
%%%
%%% The server will respond to a CONNECT command with either an error or
%%% the appropriately encoded version of {\"version\":\"1.0\"}
%%%
%%%   ``` <<17:8/integer-unit:8, <<"{\"version\":1.0}">> = 
%%%       <0,0,0,0,0,0,0,17,123,34,118,101,114,115,105,111,110,34,
%%%        58,34,49,46,48,34,125>> '''
%%%
%%%
%%% The CLOSE command closes the socket, ending the session
%%%
%%% ```M = <<"{close}">>''' 
%%%
%%%  Size is 7 so the CLOSE message is:
%%%  ```<0,0,0,0,0,0,0,7,123,99,108,111,115,101,125>> '''
%%%
%%%
%%% COMMAND messages are string versions of the tuple-messages which 
%%% {@link jc_bridge. jc_bridge} uses, only without the self() parameter. For 
%%% example the jc_brdige message, {self(), {put, Map, Key, Value}} becomes 
%%% {put, Map, Key, Value}
%%%
%%% The RESPONSE will be an appropriately encoded, binary version of a JSON 
%%% response representing the Erlang return value. 
%%% 
%%% A client session might look  as follows:
%%%
%%%     ```client:send("{put, evs, 1, \"a string value\"}")
%%%     ==> <<"{\"ok\":1}">>            
%%%
%%%     client:send("{get, evs, 1}"),
%%%     ==> <<"{\"ok\": \"a string value\"}">>'''
%%%
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

-record(jc_p, {socket, 
	       trans                 :: 'jc_protocol', 
	       connected = false     :: boolean(),
	       acc       = undefined :: undefined | binary(),
	       size      = undefined :: undefined | non_neg_integer(),
	       version   = <<"1.0">> :: binary()}).



%%% ============================================================================
%%% Modue API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Start the gen_server which will handle the socket.
%%
-spec start_link(ranch:ref(), any(), jc_protocol, any()) -> {ok,pid()}. 

start_link(Ref, Socket, Trans, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Trans, Opts]).



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
    ok = T:setopts(S, [{active, once}, {packet, 0}]),
    lager:info("~p(~p): up and listening on port ~p.",
	       [?MODULE, self(), Port]),

    gen_server:enter_loop(?MODULE, [],
			  #jc_p{socket = S, 
				trans = T,
				connected = false,
				size = undefined,
				acc = <<>>},
			  ?TIMEOUT).


%% -----------------------------------------------------------------------------
%% @private Handle info messages: Socket messages and jc_psub subscription 
%% messages.
%%
-spec handle_info(any(), #jc_p{}) -> {noreply, #jc_p{}}.

handle_info({tcp, S, Data}, State = #jc_p{socket=S, trans=T})->
    T:setopts(S, [{active, once}]),
    NewState = handle_data(Data, State),
    {noreply, NewState, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State = #jc_p{socket=S, trans = T}) ->
    T:send(S, marshal(Msg)),
    {noreply, State, ?TIMEOUT}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages.
%%
-spec handle_call(term(), {pid(), _}, #jc_p{}) -> {reply, ok, #jc_p{}}.

handle_call(Request, _From, State) ->
    lager:warning("~p: unrecognized handle_call request: ~p.",
		  [?MODULE, Request]),
	{reply, ok, State}.


%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_p{}) -> {noreply, #jc_p{}}.

handle_cast(Msg, State) ->
    lager:warning("~p: unexpected cast message: ~p.", [?MODULE, Msg]),
    {noreply, State}.


%% -----------------------------------------------------------------------------
%% @private Terminate server.
%%
-spec terminate(any(), #jc_p{}) -> any().

terminate(Reason, _State) ->
    lager:warning("~p: terminated with reason: ~p.", [?MODULE, Reason]),
    ok.


%% -----------------------------------------------------------------------------
%% @private Convert process state when code is changed.
%%
-spec code_change(term(), #jc_p{}, any()) -> {ok, #jc_p{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @private Top-level handler for socket data, returns a new state.
%%
-spec handle_data(Data::binary(), OldState::#jc_p{}) -> NewState::#jc_p{}.

handle_data(Data, #jc_p{trans = T, socket = S, connected = C} = State) ->
    try
	handle(Data, State)
    catch
	throw:{fatal, F} ->
	    T:send(S, marshal({error, {protocol_error, F}})),
	    self() ! {tcp_closed, S},
	    State;
	_:Error ->
	    T:send(S, marshal({error, Error})),
	    State#jc_p{acc = <<>>,
		       size = undefined,
		       connected=C}
    end.
	

%% -----------------------------------------------------------------------------
%% Get Size Header, Get size number of bytes, Execute Command, Repeat. When no
%% more bytes, save what has been retrieved thus far in the State and return.
%%
-spec handle(Data::binary(), OldState::#jc_p{}) -> NewState::#jc_p{}.

handle(<<>>, State) ->
    State;

handle(<<Size:8/integer-unit:8,C/binary>>,#jc_p{size=undefined, acc = <<>>}=S)->
    case byte_size(C) of
	CSize when CSize >= Size ->
	    <<Command:Size/binary, Remainder/binary>> = C,
	    S2 = execute(Command, S),
	    handle(Remainder, S2);
	Less ->
	    S#jc_p{size = Size - Less, acc = C}
    end;

handle(Data, #jc_p{size = undefined, acc = Acc} = S) ->
    NewData = <<Acc/binary, Data/binary>>,
    case byte_size(NewData) of
	NSize when NSize >= 8 ->
	    <<Size:8/integer-unit:8, Remainder/binary>> = NewData,
	    handle(Remainder, S#jc_p{size=Size, acc = <<>>});
	_ ->
	    S#jc_p{acc = NewData, size = undefined}
    end;

handle(Data, #jc_p{acc = Acc, size = Size} = S) when size /= undefined->
    case byte_size(Data) of 
	BSize when BSize >= Size ->
	    <<Seg:Size/binary, Remainder/binary>> = Data,
	    Command = <<Acc/binary, Seg/binary>>,
	    S2 = execute(Command, S),
	    handle(Remainder, S2#jc_p{acc = <<>>, size = undefined});
	Less ->
	    S#jc_p{size = Size - Less, acc = <<Acc/binary, Data/binary>>}
    end.


%% -----------------------------------------------------------------------------
%% Execute the command.
%
execute(Command, #jc_p{trans=T, socket=S, connected=false} = State)->
    case evaluate(Command) of
	open_session ->
	    lager:debug("~p (~p): connected with version: ~p",
			[?MODULE, self(), <<"1.0">>]),
	    T:send(S, marshal({version, 1.0})),
	    State#jc_p{connected = true};
	_ ->
	    throw({fatal, missing_connect})
   end;

execute(Command, #jc_p{trans=T, socket = S} = State) ->
    case evaluate(Command) of
	close_session -> 
	    T:send(S, marshal({close, 1.0})),
	    self() ! {tcp_closed, S};
	{command, R} ->
	    jc_bridge:do(R),
	    State;
	_ ->
	    throw(bad_command)
    end.


% ==============================================================================
% Utility functions
% ==============================================================================

	       
% ------------------------------------------------------------------------------
% Given a string representation of a tuple, convert it to an Erlang tuple, make
% strings binary and determine the cache action implied by the Command.
%
evaluate(Command) ->
    try
	{ok,Scanned,_} = erl_scan:string(binary_to_list(<<Command/binary,".">>)),
	{ok,Parsed} = erl_parse:parse_exprs(strings_to_binary(Scanned, [])),
	determine_action(Parsed)
    catch
	_:_ ->     
	    throw(command_syntax)
    end.


determine_action([{tuple,1,[{atom,1,close}]}]) ->
    close_session;

determine_action([{tuple,1,[{atom,1,connect},{tuple,1,
					      [{atom,1,version},
					       {bin,1,
						[{bin_element,1,
						  {string,1,"1.0"},
						  default,default}]}]}]}]) ->
    open_session;

determine_action([{tuple,1,_}] = AST) ->
    {value, R, _} = erl_eval:exprs(AST, []),
    {command, R};

determine_action(_) ->
    {error, badarg}.


% Marshal the message: make it binary JSON.
marshal({ok,{H, M}}) ->
    package(jsx:encode([{hits, H}, {misses, M}]));

marshal({X, Y}) ->
    package(jsx:encode([{X,Y}]));

marshal(M) ->
    package(jsx:encode(M)).


package(Message) ->
    Size = byte_size(Message),
    <<Size:8/integer-unit:8, Message/binary>>.
		

% Walk the scanned list of tokens making strings binary strings
strings_to_binary([], Acc) ->
    lists:reverse(Acc);

strings_to_binary([{string, _, _}=S|Tl], Acc) ->
    strings_to_binary(Tl, [{'>>',1}, S, {'<<',1}|Acc]);

strings_to_binary([Hd|Tl],Acc) ->
    strings_to_binary(Tl, [Hd|Acc]).
