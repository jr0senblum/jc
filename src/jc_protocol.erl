%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%%
%%% JC Protocol 1.0
%%% This is a binary-encoded, string protocol used to provide socket-based
%%% interoperability with JC. Incoming messages are string representation
%%% of a tuple, responses are JSON.
%%%
%%% The protocol defines three message types: CONNECT, CLOSE and COMMAND all 
%%% of which are binary strings consisting of an 8 byte, big endian, unsigned
%%% integer size of bytes followed by the actual command details.
%%%
%%% Responses are also binary strings with an 8 byte, big endian, unsigned,
%%% integer size prefix.
%%%
%%% The CONNECT command initiates a session, 
%%%
%%% ```M = <<"{connect,{version,\"1.0\"}}">>''' 
%%%
%%% The byte size is 26, so the CONNECT message is:
%%% ```<<26:8/integer-unit:8, M/binary>> = 
%%%    <<0,0,0,0,0,0,0,26,123,99,111,110,110,101,99,116,44,123,118,101,114,
%%%      115,105,111,110,44,32,34,49,46,48,34,125,125>> '''
%%%
%%% The server will respond to a CONNECT command with either an error or
%%% the encoded version of {\"version\":\"1.0\"}
%%%   ``` <<17:8/integer-unit:8, <<"{\"version\":1.0}">> = 
%%%       <0,0,0,0,0,0,0,17,123,34,118,101,114,115,105,111,110,34,
%%%        58,34,49,46,48,34,125>> '''
%%%
%%%
%%% The CLOSE command closes the socket ending the session
%%%
%%% ```M = <<"{close}">>''' 
%%%
%%%  Size is 7 so the CLOSE message is:
%%%  ```<0,0,0,0,0,0,0,7,123,99,108,111,115,101,125>>
%%%
%%%
%%% COMMAND messages are string versions of the tuple-messages which 
%%% {@link jc_bridge. jc_bridge} uses only without the self() parameter. For 
%%% example {self(), {put, Map, Key, Value}} becomes 
%%% {put, Map, Key, Value}
%%%
%%% The return will be an encoded version of a string representation of the 
%%% Erlang return value. A client session might look as follows:
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
    ok = T:setopts(S, [{active, once}, {packet,0}]),
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

terminate(_Reason, _State) ->
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


%% =============================================================================
%% Top-level protcol parser, returns a new state.
%%
handle_data(Binary, #jc_p{trans = T, socket = S} = State) ->
    try
	do_command(Binary, State)
    catch
	throw:{fatal, F} ->
	    T:send(S, marshal({error, {protocol_error, F}})),
	    self() ! {tcp_closed, S};
	_:E ->
	    T:send(S, marshal({error, E})),
	    reset_state(State)
    end.
	

%% -----------------------------------------------------------------------------
%% If we have nothing yet, get the size prefix and start accumulating the 
%% command from the Socket. Otherwise, we have a part of a command from a
%% previous socket communication and we continue accumulating.
%%
do_command(<<>>, State) ->
    State;

do_command(<<Size:8/integer-unit:8, C/binary>>, #jc_p{size = undefined, acc = <<>>} = S) ->
    case byte_size(C) of
	CSize when CSize >= Size ->
	    <<Command:Size/binary, Remainder/binary>> = C,
	    S2 = execute_command(Command, S),
	    do_command(Remainder, S2);
	Less ->
	    S#jc_p{size = Size - Less, acc = C}
    end;

do_command(Bin, #jc_p{size = undefined, acc = Acc} = S) ->
    NewBin = <<Acc/binary, Bin/binary>>,
    case byte_size(NewBin) of
	NSize when NSize >= 8 ->
	    <<Size:8/integer-unit:8, Remainder/binary>> = NewBin,
	    do_command(Remainder, S#jc_p{size=Size, acc = <<>>});
	_ ->
	    S#jc_p{acc = NewBin, size = undefined}
    end;

do_command(Bin, #jc_p{acc = Acc, size = Size} = S) when size /= undefined->
    case byte_size(Bin) of 
	BSize when BSize >= Size ->
	    <<Seg:Size/binary, Remainder/binary>> = Bin,
	    Command = <<Acc/binary, Seg/binary>>,
	    S2 = execute_command(Command, S),
	    do_command(Remainder, S2#jc_p{acc = <<>>, size = undefined});
	Less ->
	    S#jc_p{size = Size - Less, acc = <<Acc/binary, Bin/binary>>}
    end.

%% -----------------------------------------------------------------------------
%% If we have a real command (size left = 0), do it.
%
execute_command(Com, #jc_p{trans=T, socket=S, connected=false} = State)->
    case evaluate(Com) of
	open_session ->
	    lager:debug("~p (~p): connected with version: ~p",
			[?MODULE, self(), <<"1.0">>]),
	    T:send(S, marshal({version, 1.0})),
	    State#jc_p{connected = true};
	_ ->
	    throw({fatal, missing_connect})
   end;

execute_command(Com, #jc_p{socket = S} = State) ->
    case evaluate(Com) of
	close_session -> 
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
% strings binary strings and determine the cache action implied by the Command.
%
evaluate(Command) ->
    try
	{ok,Scanned,_} = erl_scan:string(binary_to_list(<<Command/binary, ".">>)),
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



%% After a COMMAND frame, reset the CONNECTION (state) for next COMMAND frame.
reset_state(S) ->
    S#jc_p{acc = <<>>,
	   size = undefined,
	   connected=true}.


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
