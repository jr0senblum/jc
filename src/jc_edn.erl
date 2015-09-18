-module(jc_edn).

-export([to_edn/1]).


-spec to_edn(term()) -> binary().
to_edn(Term) ->
    iolist_to_binary(edn(Term)).


edn({error, badarg}) ->
    "{:error :badarg}";

edn(ok) ->
    ":ok";

edn({ok, not_exist}) ->
    ":not_exist";

edn(true) ->
    "true";

edn(false) ->
    "false";

edn(miss) ->
    ":miss";

edn({ok, []}) ->
    ["()"];

edn({ok, {cnt, Cnt}}) ->
    ["{:count ", edn_term(Cnt), "}"];

edn({records, Cnt}) ->
    ["{:records ", edn_term(Cnt), "}"];

edn({ok, {key, K}}) ->
    ["{:key ", edn_term(K), "}"];

edn({ok, {value, K}}) ->
    ["{:value ", edn_term(K), "}"] ;

edn({ok, {H, M}}) when is_list(H), is_list(M) ->
    ["{:hits (", edn_term(H), ") :misses (", edn_term(M), ")}"];

edn({ok, [T|_]=L}) when is_tuple(T) ->
    ["(", edn_term(L), ")}"].

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
    ["{:", Key, " ", Value, "}"];	   

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



