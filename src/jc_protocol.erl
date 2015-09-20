%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%%
%%% JC Protocol 1.0
%%% This is an binary-encoded, Edn string protocol used to provide socket-based
%%% interoperability with JC. The protocol utilizes Edn, 
%%% a string-based data notation -- see https://github.com/edn-format/edn.
%%% 
%%% The protocol defines two message types CONNECT and COMMAND which are 
%%% binary strings consisting of an 8 byte size followed by CONNECT or
%%% COMMAND details.
%%%
%%% All responses are similarly binary strings with an 8 byte size prefix.
%%%
%%% The CONNECT command initiates a session, 
%%% M = <<"(:connect {:version 1.0})">> 
%%% Size is 25, so the CONNECT message is::
%%% <<25:8, M/binary>> = 
%%% <<25,40,58,99,111,110,110,101,99,116,32,123,58,118,101,
%%%   114,115,105,111,110,32,49,46,48,125,41>>
%%%
%%% The server will respond to a CONNECT command with either an error or
%%% the encode version of {:version 1.0}
%%% <<13:8, {:version 1.0}/binary>> = 
%%% <<13,123,118,101,114,115,105,111,110,32,49,46,48,125>>
%%% 
%%% COMMAND messages consist of an 8 bytes prefix  followed by the command.
%%%
%%% NOTICE THAT KEYWORDS IN EDN will be convereted to atoms for erlang. Thus,
%%% Module and Function names must be Edn keywords. The form of a command
%%% message musbt be an Edn list with keywords specifying the desired Module
%%% and function, as in: (:module, :fn (args))
%%% (:jc :put (:evs 1 "a string value"))
%%%
%%% A client session might look as follows
%%% client:send("(:jc :put (:evs 1 \"a string value\"))").
%%% ==> {:key 1}
%%%
%%% client:send("(:jc :get (:evs 1))").
%%% ==> {:value "a string value"}
%%%
%%% Edn commands map directly to cache functions with the exception of the 
%%% jc_psub subscription functions which do NOT need a self() parameter since
%%% the per-session, tcp listener is the process which subscribes. So:
%%%
%%% client:send("(:jc_psub :map_subscribe (:evs :any :any))").
%%% ===> ok
%%%
%%% upon update to evs the client receives, 
%%% {:map_event {:map :evs :key 1 :op delete}}
%%% {:map_event {:map :evs :key 1 :op write :value :1}}
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
    Edn = binary_to_list(Com),

    case element(2, erldn:parse_str(Edn)) of
	[connect, {map, [{version, Version}]}] ->
	    lager:debug("~p (~p): connected with version: ~p",
			[?MODULE, self(), {Version}]),
	    T:send(S, marshal({version, <<"1.0">>})),
	    parse_ballance(B, State#jc_p{connected = true});
	_ ->
	    throw({fatal, bad_connect_frame})
    end;

% Parse and execute the COMMAND.
parse(B, #jc_p{trans = T, socket = S, command = Com} = State) ->
    lager:debug("~p (~p): executing command: ~p",  [?MODULE, self(), Com]),

    Edn = binary_to_list(Com),
    [M, F, A] = erldn:to_erlang(element(2, erldn:parse_str(Edn))),
    A2 = case F of
	     _ when F == map_subscribe;
		    F == map_unsubscribe;
		    F == topic_subscribe;
		    F == topic_unsubscribe ->
		 [self()| A];
	     _ ->
		 A
	 end,
    R = apply(M, F, A2),
    T:send(S, marshal(R)),
    parse_ballance(B, State).


parse_ballance(<<>>, State) ->
    reset_state(State);
parse_ballance(B, State) ->
    <<Size:8, B2/binary>> = B,
    get_command(B2, <<>>, State#jc_p{command = <<>>, size=Size}).



% ==============================================================================
% Utility functions
% ==============================================================================


%% After a COMMAND frame, reset the CONNECTION (state) for next COMMAND frame.
reset_state(S) ->
    S#jc_p{command = <<>>,
	   size = undefined,
	   connected=true}.


% Marshal the messate 
marshal(Message) ->
    Bin = iolist_to_binary(jc_edn:to_edn(Message)),
    Size = byte_size(Bin),
    <<Size:8, Bin/binary>>.


