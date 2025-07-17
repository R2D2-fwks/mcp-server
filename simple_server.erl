-module(simple_server).
-export([start/0]).

start() ->
    %% Load env file
    {ok, EnvLines} = file:read_file(".env"),
    Lines = string:split(binary_to_list(EnvLines), "\n", all),
    Port = 8080

    io:format("Starting server on port ~p~n", [Port]),

    %% Start inets (HTTP server)
    application:start(inets),
    inets:start(httpd, [
        {port, Port},
        {server_name, "simple_erlang_server"},
        {server_root, "."},
        {document_root, "."},
        {modules, [mod_hello]}
    ]).
