-module(hotline_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ok = (catch load_config()),
    
    {ok, { {one_for_one, 1, 1}, [
        ?CHILD(hotline_c2s, worker),
        {hotline_web, {hotline_web, start, []}, permanent, 5000, worker, dynamic}
    ]} }.

load_config() ->
    %% First, load stuff from priv conf
    Etc = code:priv_dir(hotline) ++ "/conf",
    ConfigFiles = filelib:wildcard(Etc ++ "/*.conf"),
    lists:foreach(fun handle_config/1, ConfigFiles),
    %% Second, overwrite with anything found in /etc/irccloud/*.conf
    ConfigFilesLive = filelib:wildcard("/etc/hotline/*.conf"),
    lists:foreach(fun handle_config/1, ConfigFilesLive),
    ok.

handle_config(FileFull) ->
    case file:consult(FileFull) of
        {ok, Terms} ->
            lists:foreach(fun({K, V})->
                application:set_env(hotline, K, V)
            end, Terms),
            ok;
        
        {error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = lists:flatten(FileFull ++ " : " ++ file:format_error(Reason)),
            io:format("ERROR Problem parsing config file ~n~s~n", [ExitText]),
            throw(config_error);
        
        {error, Reason} ->
            ExitText = lists:flatten(FileFull ++ ": " ++ file:format_error(Reason)),
            io:format("ERROR Problem parsing config file ~n~s~n", [ExitText]),
            throw(config_error)
    end.
