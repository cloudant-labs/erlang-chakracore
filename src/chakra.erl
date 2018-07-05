-module(chakra).

-compile(no_native).
-on_load(init/0).

-export([
    create_runtime/0,
    create_runtime/1,
    memory_usage/1,
    gc/1,
    enable/1,
    disable/1,
    interrupt/1,

    create_context/0,
    create_context/1,
    run/2,
    call/3,
    idle/1
]).


-type runtime_opt() :: [
    {memory_limit, integer()}
    | disable_background_work
    | allow_script_interrupt
    | enable_idle_processing
    | disable_native_code_generation
    | disable_eval
    | enable_experimental_features
].


-spec create_runtime() -> {ok, reference()} | {error, atom()}.
create_runtime() ->
    create_runtime([]).


-spec create_runtime([runtime_opt()]) -> {ok, reference()} | {error, atom()}.
create_runtime(Options) when is_list(Options) ->
    nif_create_runtime(Options).


memory_usage(Ctx) ->
    nif_memory_usage(Ctx).


gc(Ctx) ->
    nif_gc(Ctx).


enable(Ctx) ->
    nif_enable(Ctx).


disable(Ctx) ->
    nif_disable(Ctx).


interrupt(Ctx) ->
    nif_interrupt(Ctx).


-spec create_context() -> {ok, reference()} | {error, atom()}.
create_context() ->
    {ok, Rt} = create_runtime(),
    create_context(Rt).


-spec create_context([runtime_opt()] | reference()) ->
            {ok, reference()} | {error, atom()}.
create_context(Runtime) when is_reference(Runtime) ->
    nif_create_context(Runtime);
create_context(Options) when is_list(Options) ->
    {ok, Rt} = create_runtime(Options),
    nif_create_context(Rt).


run(Ctx, Script) ->
    nif_run(Ctx, Script).


call(Ctx, Name, Args) when is_list(Args) ->
    nif_call(Ctx, Name, Args).


idle(Ctx) ->
    nif_idle(Ctx).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "chakra"), 0).


nif_create_runtime(_Options) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_memory_usage(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_gc(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_enable(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_disable(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_interrupt(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_create_context(_Options) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_run(_Ctx, _Script) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_call(_Ctx, _Name, _Args) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_idle(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).
