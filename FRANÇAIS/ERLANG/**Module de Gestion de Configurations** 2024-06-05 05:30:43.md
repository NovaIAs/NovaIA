**Module de gestion des configurations**

```erlang
-module(config).

-export([
    get/1,
    get/2,
    set/2,
    delete/1
]).

-record(config, {
    key,
    value
}).

-spec get(Key :: String()) -> {ok, Value} | {error, not_found}.
get(Key) ->
    {ok, Config} = ets:lookup(config, Key),
    {ok, Config#config.value}.

-spec get(Key :: String(), Default :: any()) -> Value.
get(Key, Default) ->
    case get(Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.

-spec set(Key :: String(), Value :: any()) -> ok.
set(Key, Value) ->
    ets:insert_new(config, #config{key = Key, value = Value}).

-spec delete(Key :: String()) -> ok.
delete(Key) ->
    ets:delete(config, Key).
```

**Module d'API REST**

```erlang
-module(api).

-export([
    start_api/0,
    handle_request/2
]).

start_api() ->
    application:start(cowboy),
    {ok, _} = cowboy:start_clear(#{
        ip => {0, 0, 0, 0},
        port => 8080,
        env => [{dispatch, dispatch}]
    }, [{env, [{dispatch, dispatch}]}, {router, router}]).

handle_request(Req, State) ->
    case cowboy_req:path(Req) of
        ["config"] ->
            config_handler:handle_request(Req, State);
        _ ->
            cowboy_req:reply(404, [], "Not found", Req)
    end.
```

**Module de gestion des configurations HTTP**

```erlang
-module(config_handler).

-export([
    handle_request/2
]).

handle_request(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            handle_get_request(Req, State);
        <<"POST">> ->
            handle_post_request(Req, State);
        <<"DELETE">> ->
            handle_delete_request(Req, State);
        _ ->
            cowboy_req:reply(400, [], "Bad request", Req)
    end.

handle_get_request(Req, State) ->
    Key = cowboy_req:binding(id, Req),
    case config:get(Key) of
        {ok, Value} ->
            cowboy_req:reply(200, [], Value, Req);
        _ ->
            cowboy_req:reply(404, [], "Not found", Req)
    end.

handle_post_request(Req, State) ->
    {ok, Body, _} = cowboy_req:body(Req),
    {Key, Value} = jiffy:decode(Body),
    config:set(Key, Value),
    cowboy_req:reply(201, [], "Created", Req).

handle_delete_request(Req, State) ->
    Key = cowboy_req:binding(id, Req),
    config:delete(Key),
    cowboy_req:reply(204, [], "", Req).
```