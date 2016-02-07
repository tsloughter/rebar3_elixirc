-module(rebar3_elixirc_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ex},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 ex compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin for compiling elixir apps"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,

    application:ensure_all_started(elixir),

    [begin
         Opts = rebar_app_info:opts(AppInfo),

         CompilerOpts = rebar_opts:get(Opts, ex_opts, #{}),
         elixir_config:put(compiler_options, CompilerOpts),

         OutDir = list_to_binary(rebar_app_info:ebin_dir(AppInfo)),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "lib"),
         FoundFiles = [list_to_binary(X) || X <- rebar_utils:find_files(SourceDir, ".*\\.ex\$")],

         'Elixir.Kernel.ParallelCompiler':files_to_path(FoundFiles, OutDir)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
