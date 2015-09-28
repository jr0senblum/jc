%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%%
%%% JC Protocol 1.0
%%% This is an binary-encoded, string protocol used to provide socket-based
%%% interoperability with JC. 
%%% 
%%% The protocol defines two message types CONNECT and COMMAND which are 
%%% binary strings consisting of an 8 byte size followed by CONNECT or
%%% COMMAND details.
%%%
%%% All responses are similarly binary strings with an 8 byte size prefix.
%%%
%%% The CONNECT command initiates a session, 
%%%
%%% ```M = <<"(:connect {:version 1.0})">>''' 
%%% Size is 25, so the CONNECT message is::
%%% ```<<25:8, M/binary>> = 
%%%    <<25,40,58,99,111,110,110,101,99,116,32,123,58,118,101,
%%%      114,115,105,111,110,32,49,46,48,125,41>> '''
%%%
%%% The server will respond to a CONNECT command with either an error or
%%% the encode version of {version, 1.0}
%%% ```<<14:8, {version, 1.0}/binary>> = 
%%%    <<14,123,118,101,114,115,105,111,110,44,32,49,46,48,125>''
%%% 
%%% COMMAND messages consist of an 8 bytes prefix  followed by the command.
%%%
%%% The command string is the binary string version of messages used with jc_bridge
%%% without the self() parameter. The return string is a string versino of the Erlan return values.
%%% A client session might look as follows
%%%
%%% client:send("{put, evs, 1 \"a string value\"}")
%%% ==> <<"{ok,{key, 1}}">>
%%%
%%% client:send("{get, evs, 1}"),
%%% ==> <<"{ok,{value \"a string value\"}}">>
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
-define(DIG(D), 
	D >= <<$0>> andalso 
	D=< <<$9>>).

-record(jc_p, {socket, 
			    trans           :: 'jc_protocol', 
			    connected = false   :: boolean(),
			    command = undefined :: undefined |
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
    ok = T:setopts(S, [{active, once}]),
    lager:debug("~p(~p): up and listening on ~p.",
	       [?MODULE, self(), Port]),

    gen_server:enter_loop(?MODULE, [],
			  #jc_p{socket = S, 
					trans = T,
					connected = false,
					size = undefined,
					command = <<>>},
			  ?TIMEOUT).

%% -----------------------------------------------------------------------------
%% @private Handle info messages: Socket messages and jc_psub subscription 
%% messages.
%%
-spec handle_info(any(),#jc_p{}) -> {noreply,#jc_p{}}.

handle_info({tcp, S, Data}, State = #jc_p{socket=S, trans=T})->

    NewState = protocol(Data, State),
    T:setopts(S, [{active, once}]),
    {noreply, NewState, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State = #jc_p{socket=S, trans = T}) ->
    io:format("received ~p~n", [Msg]),
    T:send(S, marshal(Msg)),
    {noreply, State, ?TIMEOUT}.


%% -----------------------------------------------------------------------------
%% @private Hande call messages.
%%
-spec handle_call(term(), {pid(), _}, #jc_p{}) -> 
			                         {reply, ok, #jc_p{}}.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.



%% -----------------------------------------------------------------------------
%% @private Handle cast messages.
%%
-spec handle_cast(any(), #jc_p{}) -> {noreply, #jc_p{}}.

handle_cast(_Msg, State) ->
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
-spec code_change(term(), #jc_p{}, any()) -> 
			                              {ok, #jc_p{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% =============================================================================
%% Top-level protcol parser
%%
protocol(B, #jc_p{trans = T, socket = S} = State) ->
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
	

%% Not CONNECTED, try to get the connect frame,
%% repat,
%%
proto(B, #jc_p{command = <<>>}=State) ->
  
    <<Size:8, C/binary>> = B,
    get_command(C, <<>>, State#jc_p{size=Size});

proto(B, #jc_p{command=Com}=State) ->
    get_command(B, Com, State).


% Walk the binary accumulating characters until we have grabbed the adveritesed
% amount. Execute the command, take care of any bytes left over.
%
get_command(B, Acc, #jc_p{size = 0} = S) ->
    parse(B, S#jc_p{command = Acc});

get_command(<<>>, Acc, S) ->
    S#jc_p{command=Acc};

get_command(<<C:1/binary, B/binary>>, Acc, #jc_p{size=Size}=S) when Size > 0 ->
    get_command(B, <<Acc/binary, C/binary>>, S#jc_p{size = Size-1}).


parse(B, #jc_p{trans = T, socket = S, command = Com, connected = false}=State)->
    case eval(Com) of
	open_session ->
	    lager:debug("~p (~p): connected with version: ~p",
			[?MODULE, self(), <<"1.0">>]),
	    T:send(S, marshal(<<"{version, 1.0}">>)),
	    parse_ballance(B, State#jc_p{connected = true});
	_ ->
	    throw({fatal, bad_connect})
   end;

% Parse and execute the COMMAND.
parse(B, #jc_p{socket = S, command = Com} = State) ->
    lager:debug("~p (~p): executing command: ~p~n.",  [?MODULE, self(), Com]),

    case eval(Com) of
	close_session -> 
	    self() ! {tcp_closed, S};
	{command, _R} ->
	    parse_ballance(B, State);
	_ ->
	    throw({fatal, bad_command})
    end.


parse_ballance(<<>>, State) ->
    reset_state(State);
parse_ballance(B, State) ->
    <<Size:8, B2/binary>> = B,
    get_command(B2, <<>>, State#jc_p{command = <<>>, size=Size}).



% ==============================================================================
% Utility functions
% ==============================================================================


eval(Command) ->
    {ok,Scanned,_} = erl_scan:string(binary_to_list(<<Command/binary, ".">>)),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    case Parsed of
	[{tuple,1,[{atom,1,close}]}] ->
	    close_session;
	[{tuple,1,
	  [{atom,1,connect},{tuple,1,[{atom,1,version},{string,1,"1.0"}]}]}] ->
	    open_session;
	AST ->
	    AST2 = make_ast(AST),
	    {value, R, _} = erl_eval:exprs(AST2, []),
	    {command, R}
    end.


% Convet the AST to one that sends jc_bridge the command
% {Op, P1, P2} becomes jc_bridge ! {self(), {Op, P1, P2}}
%
make_ast(AST)->
    [{tuple, 1, R}] = AST,
    [{op,1,'!',
      {atom,1,jc_bridge},
      {tuple,1,
       [{call,1,{atom,1,self},[]},
	{tuple,1,
	 R}]}}].


%% After a COMMAND frame, reset the CONNECTION (state) for next COMMAND frame.
reset_state(S) ->
    S#jc_p{command = <<>>,
	   size = undefined,
	   connected=true}.


% Marshal the message: make it a binary string representation.
marshal(M) when is_binary(M)-> 
    package(M);
marshal(M) ->
    package(list_to_binary(io_lib:format("~p",[M]))).

package(Message) ->
    io:format("I have ~s~n",[Message]),
    Size = byte_size(Message),
    <<Size:8, Message/binary>>.
		





