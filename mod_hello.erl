-module(mod_hello).
-export([do/1]).

do(_Request) ->
    {
        ok,
        200,
        [{"Content-Type", "text/plain"}],
        "Hello from Erlang!"
    }.
