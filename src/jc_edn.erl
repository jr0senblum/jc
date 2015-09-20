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
    iolist_to_binary(edn(Term)).


edn({error, E}) ->
    ["{:error ", edn_term(E), "}"];
edn(ok) ->
    ":ok";
edn({ok, not_exist}) ->
    ":not_exist";
edn({version, V}) ->
    ["{:version ", V,"}"];
edn(true) ->
    "true";
edn(false) ->
    "false";
edn(miss) ->
    ":miss";


edn({topic_event, {Topic, E}}) ->
    ["{:topic_event {:topic ", edn(Topic), " :event ", edn(E),"}}"];

edn({map_event, {Map, Key, delete}}) ->
    ["{:map_event {:map ", edn_term(Map),
      " :key ", edn_term(Key),
      " :op delete}}"];

edn({map_event, {Map, Key, write, V}}) ->
    ["{:map_event {:map ", edn_term(Map),
      " :key ", edn_term(Key),
      " :op write", 
      " :value :", edn_term(V), "}}"];

edn({uptime, [{up_at, U},{now, N}, {up_time, {D, {H, M, S}}}]}) ->
    ["{:uptime {:up_at ", edn_term(list_to_binary(U)), 
              " :now ", edn_term(list_to_binary(N)),
              " :up_time {:days ", edn_term(D),
                           " :hours ", edn_term(H),
                           " :minutes ", edn_term(M),
                           " :seconds ", edn_term(S), "}}}"];
    
edn({nodes, {active, A}, {configured, C}}) ->
    ["{:active (", edn_term(A), ") :configured (", edn_term(C), ")}"];

edn({sizes, Details}) ->
    ["{:size (", 
     [["{",edn_term(Name), 
      " {", edn_term(Cnt), " ", edn_term(Words),"}} "] 
      || {Name, Cnt, Words} <- Details],")}"];



edn({sequence, Number}) ->
    ["{:sequence ", edn_term(Number), "}"];


edn({ok, {cnt, Cnt}}) ->
    ["{:count ", edn_term(Cnt), "}"];

edn({records, Cnt}) ->
    ["{:records ", edn_term(Cnt), "}"];

edn({maps, MapList}) ->
    ["{:maps (", edn_term(MapList), ")}"];


edn({ok, I}) when is_integer(I) ->
    [":ok ", edn_term(I)];


edn({A, L}) when is_atom(A), is_list(L) ->
    ["{:",atom_to_list(A), 
     " (", [["{", edn_term(M)," ", edn_term(S),"} "] || {M,S} <- L], ")}"];


edn({ok, {key, K}}) ->
    ["{:key ", edn_(K), "}"];

edn({ok, {value, K}}) ->
    ["{:value ", edn_(K), "}"] ;

edn({ok, {H, M}}) when is_list(H), is_list(M) ->
    ["{:hits (", edn_term(H), ") :misses (", edn_term(M), ")}"];

edn({ok, []}) ->
    ["()"];

edn({ok, H}) when is_list(H) ->
    ["(", edn_term(H), ")"];


edn({ok, [T|_]=L}) when is_tuple(T) ->
    ["(", edn_term(L), ")}"];

edn(T) -> edn_term(T).



edn_([]) ->
    "()";

edn_([_|_]=L) ->
    ["(", edn_term(L), ")"];

edn_(T) ->
    edn_term(T).


edn_term(nil) -> ["nil"];

edn_term({keyword,nil}) -> [":nil"];

edn_term({symbol, S}) -> atom_to_list(S);



edn_term({'char', C}) ->
    ["\\", C];

edn_term(D) when is_tuple(D), element(1, D) == dict  ->
    ["{", [[edn_term(K)," ", edn_term(V)," " ] || {K,V} <- D:to_list()], "}"];

edn_term(D) when is_tuple(D), element(1, D) == set  ->
    ["#{", [[edn_term(K)," "] || K <- sets:to_list(D)], "}"];

edn_term({map, L}) when is_list(L)  ->
    ["{", [[edn_term(K)," ", edn_term(V)," " ] || {K,V} <- L], "}"];

edn_term({vector, L}) when is_list(L) ->
    ["[", [[edn_term(E)," "] || E <- L], "]"];

edn_term({set, L}) when is_list(L) ->
    ["#{", [[edn_term(E)," "] || E <- L], "}"];

edn_term([]) ->
    ["()"];

edn_term([Hd|[]]) when is_list(Hd) ->
    ["(", edn_term(Hd), ")"];

edn_term([Hd|[]]) ->
    [edn_term(Hd)];

edn_term([H|T]) ->
    [edn_term(H), " ", edn_term(T)];

edn_term({K, V}) ->
    Key = edn_term(K),
    Value = edn_term(V),
    ["{", Key, " ", Value, "}"];	   

edn_term(Bin) when is_binary(Bin) -> 
    jsonx:encode(Bin);

edn_term(Term) when is_integer(Term); is_float(Term) ->
    jsonx:encode(Term); 

edn_term(true) ->
    "true";
edn_term(false) ->
    "false";
edn_term(Term) when is_atom(Term) ->
    [":", atom_to_list(Term)];

edn_term(_T) ->
    throw({error, bad_edn}).
