-module(mcp_server).
-behaviour(application).

-export([start/0, stop/0]).

% Convenience functions to start/stop the application
start() ->
    application:ensure_all_started(mcp_server).

stop() ->
    application:stop(mcp_server).
