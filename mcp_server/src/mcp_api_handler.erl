-module(mcp_api_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"POST">>, Req0, State) ->
    % Process a chat request
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    RequestParams = jsx:decode(Body, [return_maps]),
    
    % Extract preferred model (if any)
    PreferredModel = maps:get(<<"model">>, RequestParams, auto),
    
    % Process the request through the orchestrator
    Result = mcp_orchestrator:process_request(RequestParams, PreferredModel),
    
    % Return the response
    Response = case Result of
        {ok, LlmResponse} ->
            jsx:encode(#{
                status => <<"success">>,
                data => LlmResponse
            });
        {error, Reason} ->
            jsx:encode(#{
                status => <<"error">>,
                error => jsx:encode(Reason)
            })
    end,
    
    Req2 = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req2, State};

handle_request(_, Req0, State) ->
    % Method not allowed
    Req = cowboy_req:reply(405, 
        #{<<"allow">> => <<"POST">>},
        Req0),
    {ok, Req, State}.
