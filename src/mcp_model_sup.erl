-module(mcp_model_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_model/2, stop_model/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Dynamic supervisor that will manage LLM worker processes
    % Each LLM (ChatGPT, Gemini, Claude, etc.) will be a child process
    {ok, {#{strategy => simple_one_for_one, intensity => 5, period => 10}, [
        #{
            id => mcp_model,
            start => {mcp_model, start_link, []},
            restart => temporary,  % Don't restart failed models automatically
            shutdown => 5000,
            type => worker,
            modules => [mcp_model]
        }
    ]}}.

% API to start a new model worker
start_model(ModelType, Config) ->
    % ModelType: chatgpt, gemini, claude, etc.
    supervisor:start_child(?MODULE, [ModelType, Config]).

% API to stop a specific model worker
stop_model(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
