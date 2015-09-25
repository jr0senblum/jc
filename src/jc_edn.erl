%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@carelogistics.com>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc 
%%% This module converts jc cache return values to Edn -  to be put over the 
%%% wire via TCP - for JC interoperability.
%%% @end
%%% Created : 25 Aug 2015 by Jim Rosenblum <jrosenblum@carelogistics.coml>
%%% ----------------------------------------------------------------------------
-module(jc_edn).

-export([to_edn/1]).


-spec to_edn(term()) -> binary().
to_edn(Term) ->
    R = jsonx:encode(edn(Term)),
    io:format("==> ~s~n",[R]),
    R.

edn(true) -> [true];
edn(false) -> [false];

edn({topic_event, {Topic, E}}) ->
    {[{topic_event, [{topic, edn(Topic)}, {event, edn(E)}]}]};

edn({map_event, {Map, Key, delete}}) ->
    {[{map_event, [{map, edn_term(Map)},
		   {key, edn_term(Key)},
		   {op, delete}]}]};

edn({map_event, {Map, Key, write, V}}) ->
    {[{map_event, [{map, edn_term(Map)},
		   {key, edn_term(Key)},
		   {op, write},
		   {value, edn_term(V)}]}]};


edn({uptime, [{up_at, U},{now, N}, {up_time, {D, {H, M, S}}}]}) ->
    {[{uptime, [{up_at, edn_term(list_to_binary(U))}, 
		{now, edn_term(list_to_binary(N))},
		{up_time, [{days, edn_term(D)},
                           {hours, edn_term(H)},
                           {minutes, edn_term(M)},
                           {seconds, edn_term(S)}]}]}]};
    
edn({nodes, {active, A}, {configured, C}}) ->
    {[{nodes, {[{active, [edn_term(N) || N <- A]},
		{configured, [edn_term(N) || N <- C]}]}}]};
    
edn({sizes, Details}) ->
    {[{sizes, [{[{table, edn_term(Name)}, {reccords, edn_term(Cnt)},{words, edn_term(Words)}]} ||
      {Name, Cnt, Words} <- Details]}]};

edn({sequence, Number}) ->
    {[{sequence, edn_term(Number)}]};

edn({records, Cnt}) ->
    {[{records, edn_term(Cnt)}]};

edn({maps, MapList}) ->
    {[{maps, lists:flatten([edn_term(MapList)])}]};



edn({version, V}) ->
    {[{status, ok}, {version, V}]};

edn(miss) ->
    {[{status, ok}, {message, miss}]};


edn({error, E}) ->
    {[{status, error}, {message, edn_term(E)}]};


edn(ok) ->
    {[{status, ok}]};

edn({ok, not_exist}) ->
    {[{status, ok}, {message, not_exist}]};

edn({ok, {cnt, Cnt}}) -> 
    {[{status, ok}, {count, edn_term(Cnt)}]};

edn({ok, I}) when is_integer(I) -> 
    "*******************";

edn({ok, {key, K}}) ->
    {[{status, ok}, {key, edn_term(K)}]};

edn({ok, {value, V}}) ->
    {[{status, ok}, {key, edn_term(V)}]};

edn({ok, {H, M}}) when is_list(H), is_list(M) ->
    Hits = lists:flatten([edn_term(H)]),
    Misses = lists:flatten([edn_term(M)]),
    {[{status, ok}, {hits, Hits}, {misses, Misses}]};

edn({ok, []}) -> 
    {[{status, ok}, {items, []}]};


edn({ok, H}) when is_list(H) -> 
    {[{status, ok}, 
      {items, [edn_term(X) || X<-H]}]};


edn({ok, [T|_]=L}) when is_tuple(T) ->
    {[{status, ok}, [edn_term(L)]]};

edn({A, L}) when is_atom(A), is_list(L) -> 
    {[{status, ok}, [{edn_term(M), edn_term(S)} || {M,S} <- L]]};

edn(T) -> 
    edn_term(T).


edn_term(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
edn_term([]) ->
    [];

edn_term([H|[]]) when is_list(H) ->
    [edn_term(H)];

edn_term([Hd|[]]) ->
    edn_term(Hd);

edn_term([H|T]) when is_list(H) ->     
    [[edn_term(H)], edn_term(T)];

edn_term([H|T]) ->
    [edn_term(H), edn_term(T)];

edn_term({K, V}) -> 
    Key = edn_term(K),
    Value = edn_term(V),
    {[{Key, Value}]};

edn_term(I) when is_integer(I); is_float(I) ->
    I;

edn_term(true) ->
    true;
edn_term(false) ->
    false;
edn_term(B) when is_binary(B) ->
    B;

edn_term(_T) ->
    throw(bad_edn).
