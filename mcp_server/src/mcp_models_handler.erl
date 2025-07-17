-module(mcp_models_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    % Get list of available models
    {ok, Models} = mcp_orchestrator:list_models(),
    
    % Return the models list
    Response = jsx:encode(#{
        status => <<"success">>,
        models => Models
    }),
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State};

handle_request(_, Req0, State) ->
    % Method not allowed
    Req = cowboy_req:reply(405, 
        #{<<"allow">> => <<"GET">>},
        Req0),
    {ok, Req, State}.
