-module(mcp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Define the HTTP routes for our MCP server
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/chat", mcp_api_handler, []},
            {"/api/v1/models", mcp_models_handler, []}
        ]}
    ]),
    
    % Start the HTTP server
    {ok, _} = cowboy:start_clear(http, 
        [{port, application:get_env(mcp_server, port, 8080)}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    % Start the main supervisor
    mcp_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
