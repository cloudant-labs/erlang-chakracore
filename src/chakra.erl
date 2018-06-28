-module(chakra).

-compile(no_native).
-on_load(init/0).

-export([
    create_context/0,
    create_context/1,

    run/2,
    call/3,

    gc/1,
    idle/1,

    enable/1,
    disable/1,
    interrupt/1
]).


-type context_opt() :: [
    {memory_limit, integer()}
    | disable_background_work
    | allow_script_interrupt
    | enable_idle_processing
    | disable_native_code_generation
    | disable_eval
    | enable_experimental_features
].


-spec create_context() -> {ok, reference()} | {error, atom()}.
create_context() ->
    create_context([]).


-spec create_context([context_opt()]) -> {ok, reference()} | {error, atom()}.
create_context(Options) ->
    nif_create_context(Options).


run(Ctx, Script) ->
    nif_run(Ctx, Script).


call(Ctx, Name, Args) when is_list(Args) ->
    nif_call(Ctx, Name, Args).


gc(Ctx) ->
    nif_gc(Ctx).


idle(Ctx) ->
    nif_idle(Ctx).


enable(Ctx) ->
    nif_enable(Ctx).


disable(Ctx) ->
    nif_disable(Ctx).


interrupt(Ctx) ->
    nif_interrupt(Ctx).


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


nif_create_context(_Options) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_run(_Ctx, _Script) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_call(_Ctx, _Name, _Args) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_gc(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_idle(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_enable(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_disable(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).


nif_interrupt(_Ctx) ->
    erlang:nif_error(chakra_nif_not_loaded).

