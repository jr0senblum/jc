%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2018-2019, Jim Rosenblum
%%% @doc Module that manages RESTful, collection interactions (Maps and Map).
%%% This is a Cowboy handler handling GET, POST, OPTIONS, DELETE, and HEAD
%%% verbs and provides a JSON response.
%%%
%%% @version {@version}
%%% @end
%%% Created : 20 Oct 2018 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(cb_collections_h).

% Cowboy handler required function
-export([init/2]).

% RESTFUL functions
-export([allowed_methods/2,
         content_types_provided/2,
         delete_completed/2,
         delete_resource/2,
         resource_exists/2]).

% Callback that handles constructing JSON responses
-export([collection_to_json/2]).

% Handler state
-record(cb_coll_state, {op :: map | maps}).



%%% ============================================================================
%%% Module callbacks required for Cowboy handler
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Initialize state to be parameter passed into the handler indicating 
%% whether we are dealing with map or maps collection.
%%
-spec init(_,nonempty_maybe_improper_list())-> {'cowboy_rest', Req, State}
                            when Req::cowboy_req:req(),
                                 State::#cb_coll_state{}.

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #cb_coll_state{op=Op},
    {cowboy_rest, Req, State}.


%% -----------------------------------------------------------------------------
%%
-spec allowed_methods (Req, State) -> {Method, Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_coll_state{},
                                           Method::nonempty_list().

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"OPTIONS">>, <<"DELETE">>, <<"HEAD">>],
    {Methods, Req, State}.


%% -----------------------------------------------------------------------------
%% Only provide JSON for now.
%%
-spec content_types_provided(Req, State) -> {Types, Req, State}
                                            when Req::cowboy_req:req(),
                                                 State::#cb_coll_state{},
                                                 Types::cowboy:types().

content_types_provided(Req, State) ->
   Types =  [
             {<<"application/json">>, collection_to_json}
            ],
    {Types, Req, State}.


%% -----------------------------------------------------------------------------
%% Delete the resource collection - a map.
%%
-spec delete_resource(Req, State) -> {Result, Req, State}
                                     when Result::boolean(),
                                          Req::cowboy_req:req(),
                                          State::#cb_coll_state{}.

delete_resource(Req, #cb_coll_state{op = Op} = State) when Op == map ->                                          
    MapName = cowboy_req:binding(map, Req),
    ok = jc:evict_map_since(MapName,0),
    {true, Req, State};

delete_resource(Req, #cb_coll_state{op = Op} = State) when Op == maps ->                                          
    ok = jc:flush(),
    {true, Req, State}.

delete_completed(Req, #cb_coll_state{op = Op} = State) when Op == map ->                                          
    MapName = cowboy_req:binding(map, Req),
    {records, Size} = jc:map_size(MapName),
    {Size == 0, Req, State};

delete_completed(Req, #cb_coll_state{op = Op} = State) when Op == maps ->                                          
    {maps, Maps} = jc:maps(),
    {length(Maps) == 0, Req, State}.


%% -----------------------------------------------------------------------------
%%
-spec resource_exists (Req, State) -> {boolean(), Req, State}
                                      when Req::cowboy_req:req(),
                                           State::#cb_coll_state{}.

resource_exists(Req, #cb_coll_state{op = Op} = State) when Op == maps ->
    % TODO optimize this
    {maps, Maps} = jc:maps(),
    {length(Maps) > 0, Req, State};

resource_exists(Req, #cb_coll_state{op = Op} = State) when Op == map ->
    % TODO optimize this
    MapName = cowboy_req:binding(map, Req),
    {records, Records} = jc:map_size(MapName),
    {Records > 0, Req, State}.


%% -----------------------------------------------------------------------------
%% Convert jc:key_set(map) and jc:maps() responses to json and 
%%
-spec collection_to_json(Req, State) ->{Value, Req, State}
                                      when Value::nonempty_list(iodata()),
                                           Req::cowboy_req:req(),
                                           State::#cb_coll_state{}.

collection_to_json(Req, #cb_coll_state{op = map} = State) ->
    {map_collection_body(Req), Req, State};

collection_to_json(Req, #cb_coll_state{op = maps} = State) ->
    {get_or_head_maps(Req), Req, State}.



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


map_collection_body(#{method := Verb} = Req) ->
    MapName = cowboy_req:binding(map, Req),
    lager:debug("~p: ~p map ~p as JSON.",[?MODULE, Verb, MapName]),
    case Verb of
        <<"GET">> ->
            {ok, KeyList} = jc:key_set(MapName),
            SHPP = get_URI(Req),
            map_to_json(SHPP, MapName, KeyList);
        <<"HEAD">> ->
            <<>>
    end.


get_or_head_maps(#{method := Verb} = Req) ->
    lager:debug("~p: ~p maps as JSON.", [?MODULE, Verb]),
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
    [<<"{\"map\":\"">>, MapName, <<"\", \"keys\": [">>, Separated, <<"], \"links\": [{\"rel\":\"maps\",\"method\":\"GET\",\"href\":\"">>,Url,<<"\"}]}">>].


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
    [<<"{\"maps\":">>, <<"[">>, Separated, <<"], \"links\": [{\"rel\":\"self\",\"method\":\"DELETE\",\"href\":\"">>,Url,<<"\"}]}">>].
    


