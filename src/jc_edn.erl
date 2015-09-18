-module(jc_edn).

-export([to_edn/1]).


-spec to_edn(term()) -> binary().
to_edn(Term) ->
    iolist_to_binary(edn(Term)).


edn({error, badarg}) ->
    "{:error :badarg}";

edn({error, protocol}) ->
    "{:error :protocol}";

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

edn({ok, {cnt, Cnt}}) ->
    ["{:count ", edn_term(Cnt), "}"];

edn({records, Cnt}) ->
    ["{:records ", edn_term(Cnt), "}"];

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
    ["(", edn_term(L), ")}"].


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
    [":", atom_to_list(Term)].
