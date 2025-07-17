-module(mcp_model).
-behaviour(gen_server).

% API exports
-export([
    start_link/2,
    process_request/2,
    get_capabilities/1
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Define model state record
-record(state, {
    type,           % chatgpt, gemini, claude, etc.
    config,         % Configuration for this model
    capabilities,   % What this model can do
    api_client      % Reference to the API client (e.g., HTTP client)
}).

% API functions
start_link(ModelType, Config) ->
    gen_server:start_link(?MODULE, [ModelType, Config], []).

process_request(Pid, RequestParams) ->
    gen_server:call(Pid, {process_request, RequestParams}, 30000). % 30 second timeout

get_capabilities(Pid) ->
    gen_server:call(Pid, get_capabilities).

% gen_server callbacks
init([ModelType, Config]) ->
    % Initialize API client based on model type
    ApiClient = init_api_client(ModelType, Config),
    
    % Define capabilities based on model type
    Capabilities = get_model_capabilities(ModelType),
    
    % Register with the orchestrator
    mcp_orchestrator:register_model(ModelType, self()),
    
    {ok, #state{
        type = ModelType,
        config = Config,
        capabilities = Capabilities,
        api_client = ApiClient
    }}.

handle_call({process_request, RequestParams}, _From, State) ->
    % Process the request using the appropriate API client
    Result = process_request_with_client(RequestParams, State),
    {reply, Result, State};

handle_call(get_capabilities, _From, State = #state{capabilities = Capabilities}) ->
    {reply, Capabilities, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{type = ModelType}) ->
    % Unregister from the orchestrator
    mcp_orchestrator:unregister_model(ModelType),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

% Initialize the appropriate API client based on model type
init_api_client(chatgpt, Config) ->
    % Initialize OpenAI API client
    #{
        base_url => maps:get(base_url, Config, "https://api.openai.com/v1"),
        api_key => maps:get(api_key, Config),
        organization => maps:get(organization, Config, undefined)
    };
init_api_client(gemini, Config) ->
    % Initialize Google Gemini API client
    #{
        base_url => maps:get(base_url, Config, "https://generativelanguage.googleapis.com/v1"),
        api_key => maps:get(api_key, Config)
    };
init_api_client(claude, Config) ->
    % Initialize Anthropic Claude API client
    #{
        base_url => maps:get(base_url, Config, "https://api.anthropic.com/v1"),
        api_key => maps:get(api_key, Config)
    };
init_api_client(_, Config) ->
    % Generic client for other models
    #{
        base_url => maps:get(base_url, Config),
        api_key => maps:get(api_key, Config)
    }.

% Define model capabilities
get_model_capabilities(chatgpt) ->
    #{
        max_tokens => 4096,
        features => [text_completion, chat, embeddings, function_calling],
        models => [
            "gpt-3.5-turbo",
            "gpt-4",
            "gpt-4-turbo"
        ]
    };
get_model_capabilities(gemini) ->
    #{
        max_tokens => 8192,
        features => [text_completion, chat, embeddings],
        models => [
            "gemini-pro",
            "gemini-ultra"
        ]
    };
get_model_capabilities(claude) ->
    #{
        max_tokens => 100000,
        features => [text_completion, chat],
        models => [
            "claude-2",
            "claude-instant"
        ]
    };
get_model_capabilities(_) ->
    #{
        max_tokens => 2048,
        features => [text_completion],
        models => ["default"]
    }.

% Process a request with the appropriate client
process_request_with_client(RequestParams, #state{type = chatgpt, api_client = Client}) ->
    % Extract request parameters
    #{
        messages := Messages,
        model := Model
    } = RequestParams,
    
    % Build the request
    Url = maps:get(base_url, Client) ++ "/chat/completions",
    Headers = [
        {"Authorization", "Bearer " ++ maps:get(api_key, Client)},
        {"Content-Type", "application/json"}
    ],
    Body = jsx:encode(#{
        model => Model,
        messages => Messages
    }),
    
    % Make the API call
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody)};
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end;

process_request_with_client(RequestParams, #state{type = gemini, api_client = Client}) ->
    % Extract request parameters
    #{
        messages := Messages,
        model := Model
    } = RequestParams,
    
    % Convert messages to Gemini format
    GeminiMessages = convert_to_gemini_format(Messages),
    
    % Build the request
    Url = maps:get(base_url, Client) ++ "/models/" ++ Model ++ ":generateContent" ++
          "?key=" ++ maps:get(api_key, Client),
    Headers = [{"Content-Type", "application/json"}],
    Body = jsx:encode(#{
        contents => GeminiMessages
    }),
    
    % Make the API call
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody)};
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end;

process_request_with_client(RequestParams, #state{type = claude, api_client = Client}) ->
    % Extract request parameters
    #{
        messages := Messages,
        model := Model
    } = RequestParams,
    
    % Convert messages to Claude format
    ClaudeMessages = convert_to_claude_format(Messages),
    
    % Build the request
    Url = maps:get(base_url, Client) ++ "/messages",
    Headers = [
        {"X-API-Key", maps:get(api_key, Client)},
        {"Content-Type", "application/json"},
        {"Anthropic-Version", "2023-06-01"}
    ],
    Body = jsx:encode(#{
        model => Model,
        messages => ClaudeMessages,
        max_tokens => maps:get(max_tokens, RequestParams, 1024)
    }),
    
    % Make the API call
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody)};
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end;

process_request_with_client(RequestParams, #state{api_client = Client}) ->
    % Generic handler for other models
    % This would need to be customized for each specific API
    {error, {not_implemented, RequestParams, Client}}.

% Convert messages from standard format to Gemini format
convert_to_gemini_format(Messages) ->
    lists:map(
        fun(#{role := Role, content := Content}) ->
            GeminiRole = case Role of
                "user" -> "user";
                "assistant" -> "model";
                "system" -> "user" % Gemini doesn't have system, use user instead
            end,
            #{
                role => GeminiRole,
                parts => [#{text => Content}]
            }
        end,
        Messages
    ).

% Convert messages from standard format to Claude format
convert_to_claude_format(Messages) ->
    lists:map(
        fun(#{role := Role, content := Content}) ->
            ClaudeRole = case Role of
                "user" -> "user";
                "assistant" -> "assistant";
                "system" -> "system"
            end,
            #{
                role => ClaudeRole,
                content => Content
            }
        end,
        Messages
    ).
