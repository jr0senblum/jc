%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(jc_toppage_h).

-export([init/2]).

-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).

-export([collection_to_json/2]).


-record(state, {op}).



init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.


content_types_provided(Req, State) ->
   Types =  [
             {<<"application/json">>, collection_to_json}
            ],
    {Types, Req, State}.


allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"OPTIONS">>, <<"DELETE">>, <<"HEAD">>],
    {Methods, Req, State}.


resource_exists(Req, #state{op = Op} = State) when Op == maps ->
    % TODO optimize this
    {maps, Maps} = jc:maps(),
    {length(Maps) > 0, Req, State};

resource_exists(Req, #state{op = Op} = State) when Op == map ->
    % TODO optimize this
    MapName = cowboy_req:binding(map, Req),
    {records, Records} = jc:map_size(MapName),
    {Records > 0, Req, State}.




collection_to_json(Req, #state{op = map} = State) ->
    {map_collection_body(Req), Req, State};

collection_to_json(Req, #state{op = maps} = State) ->
    {get_or_head_maps(Req), Req, State}.



map_collection_body(#{method := Verb} = Req) ->
    MapName = cowboy_req:binding(map, Req),
    lager:info("~p: ~p map ~p as JSON.",[?MODULE, Verb, MapName]),
    case Verb of
        <<"GET">> ->
            {ok, KeyList} = jc:key_set(MapName),
            SHPP = get_URI(Req),
            map_to_json(SHPP, MapName, KeyList);
        <<"HEAD">> ->
            <<>>
    end.


get_or_head_maps(#{method := Verb} = Req) ->
    lager:info("~p: ~p maps as JSON.", [?MODULE, Verb]),
    case Verb of
        <<"GET">> ->
            {maps, MapList} = jc:maps(),
            SHPP = get_URI(Req),
            maps_to_json(SHPP, MapList);
        <<"HEAD">> ->
            <<>>
    end.


% ------------------------------------------------------------------------------
% Construct the URI (i.e., "HTTP://Host:Port/Path) as binary string.
%
get_URI(Req) ->
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = list_to_binary(integer_to_list(cowboy_req:port(Req))),
    Path = cowboy_req:path(Req),
    [Scheme, <<"://">>, Host, <<":">>, Port, Path, <<"/">>].


% ------------------------------------------------------------------------------
% Construct the JSON represention a mapa collection.
%
map_to_json(Url, MapName, KeyList) ->
    ListOfMaps = 
        lists:foldl(fun(Key, Acc) ->
                            [[<<"{\"key\":\"">>,Key,<<"\",">>,
                              <<"\"links\": [{\"rel\":\"self\",\"method\":\"GET\",">>,
                              <<"\"href\":\"">>, Url, Key,
                              <<"\"}]}">>]|Acc]
                    end,
                    [],
                    KeyList),
    Separated = lists:join(<<",">>,ListOfMaps),
    [<<"{\"map\":\"">>, MapName, <<"\", \"keys\": [">>, Separated, <<"]}">>].


% ------------------------------------------------------------------------------
% Construct the JSON represention the maps collection.
% {maps: [{method:GET,URL:http://host:port/path/mapname},...,{}]}
%
maps_to_json(Url, MapList) ->
    ListOfMaps = 
        lists:foldl(fun(MapName, Acc) ->
                            [[<<"{\"map\":\"">>,MapName,<<"\",">>,
                              <<"\"links\": [{\"rel\":\"self\",\"method\":\"GET\",">>,
                              <<"\"href\":\"">>, Url, MapName, 
                              <<"\"}]}">>]|Acc]
                    end,
                    [],
                    MapList),
    Separated = lists:join(<<",">>,ListOfMaps),
    [<<"{\"maps\":">>, <<"[">>, Separated, <<"], \"links\": [{\"rel\":\"self\",\"method\":\"DELETE\",\"href\":\"">>,Url,<<"flush\"}]}">>].
    


