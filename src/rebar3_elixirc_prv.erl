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
    application:ensure_all_started(mix),

    Dir = rebar_state:dir(State),
    Opts = rebar_state:opts(State),
    CompilerOpts = rebar_opts:get(Opts, ex_opts, #{}),
    elixir_config:put(compiler_options, CompilerOpts),


    [{Module, _}] = 'Elixir.Code':load_file(list_to_binary(filename:join(Dir, "mix.exs"))),
    Config = Module:project(),
    Name = proplists:get_value(app, Config),
    AppSrc = app_src_file(Config),
    AppSrcFile = filename:join([Dir, "src", atom_to_list(Name)++".app.src"]),
    Spec = io_lib:format("~p.\n", [AppSrc]),
    filelib:ensure_dir(AppSrcFile),
    file:write_file(AppSrcFile, iolist_to_binary(Spec), [raw]),

    SourceDir = filename:join(Dir, "lib"),
    FoundFiles = [list_to_binary(X) || X <- rebar_utils:find_files(SourceDir, ".*\\.ex\$")],

    filelib:ensure_dir(filename:join([Dir, "ebin", "file"])),
    'Elixir.Kernel.ParallelCompiler':files_to_path(FoundFiles, list_to_binary(filename:join(Dir, "ebin"))),

    {_, AppInfo} = rebar_app_discover:find_app(Dir, all),
    rebar_otp_app:compile(State, AppInfo),

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

app_src_file(Config) ->
    Name = proplists:get_value(app, Config),
    Desc = binary_to_list(proplists:get_value(description, Config, "")),
    Vsn = binary_to_list(proplists:get_value(version, Config, <<"0.0.1">>)),
    Deps = [D || {D, _, Opts} <- proplists:get_value(deps, Config, []),
                 not lists:member([{optional,true}], Opts), not lists:keymember(only, 1, Opts)],
    {application, Name,
     [{description, Desc},
      {vsn, Vsn},
      {modules,[]},
      {registered,[]},
      {applications,[kernel,stdlib | Deps]},
      {env,[]}]}.
