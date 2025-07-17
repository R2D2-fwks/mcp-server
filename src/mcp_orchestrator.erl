-module(mcp_orchestrator).
-behaviour(gen_server).

% API exports
-export([
    start_link/0,
    process_request/2,
    list_models/0,
    register_model/2,
    unregister_model/1
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

% Record to keep track of registered models
-record(state, {models = #{}}). % Map of ModelType -> ModelPid

% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Process a chat request with a specific model or let the orchestrator choose
process_request(RequestParams, PreferredModel) ->
    gen_server:call(?MODULE, {process_request, RequestParams, PreferredModel}).

% List all available models
list_models() ->
    gen_server:call(?MODULE, list_models).

% Register a new model
register_model(ModelType, ModelPid) ->
    gen_server:cast(?MODULE, {register_model, ModelType, ModelPid}).

% Unregister a model
unregister_model(ModelType) ->
    gen_server:cast(?MODULE, {unregister_model, ModelType}).

% gen_server callbacks
init([]) ->
    % Initialize with no models
    % Models will register themselves when they start
    {ok, #state{models = #{}}}.

handle_call({process_request, RequestParams, preferred_model}, _From, State = #state{models = Models}) ->
    % First try to use preferred model if specified and available
    case maps:find(preferred_model, Models) of
        {ok, ModelPid} when preferred_model /= auto ->
            % Use the preferred model
            Result = mcp_model:process_request(ModelPid, RequestParams),
            {reply, Result, State};
        _ ->
            % Auto-select best model or preferred model not available
            % Implement logic to choose the best model based on request parameters
            % For now, just use the first available model
            case maps:to_list(Models) of
                [] ->
                    {reply, {error, no_models_available}, State};
                [{_Type, ModelPid}|_] ->
                    Result = mcp_model:process_request(ModelPid, RequestParams),
                    {reply, Result, State}
            end
    end;

handle_call(list_models, _From, State = #state{models = Models}) ->
    % Return list of available models with their capabilities
    ModelList = maps:fold(
        fun(Type, Pid, Acc) ->
            Capabilities = mcp_model:get_capabilities(Pid),
            [{Type, Capabilities}|Acc]
        end,
        [],
        Models
    ),
    {reply, {ok, ModelList}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register_model, ModelType, ModelPid}, State = #state{models = Models}) ->
    % Monitor the model process
    _Ref = erlang:monitor(process, ModelPid),
    % Add the model to our map
    NewModels = maps:put(ModelType, ModelPid, Models),
    {noreply, State#state{models = NewModels}};

handle_cast({unregister_model, ModelType}, State = #state{models = Models}) ->
    % Remove the model from our map
    NewModels = maps:remove(ModelType, Models),
    {noreply, State#state{models = NewModels}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{models = Models}) ->
    % A model process has terminated, remove it from our map
    NewModels = maps:filter(
        fun(_Type, ModelPid) -> ModelPid /= Pid end,
        Models
    ),
    {noreply, State#state{models = NewModels}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
