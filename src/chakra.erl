-module(chakra).

-compile(no_native).
-on_load(init/0).

-export([
    create_context/0,
    create_context/1,

    run/2
]).


create_context() ->
    create_context([]).


create_context(Options) ->
    nif_create_context(Options).


run(Ctx, Script) ->
    nif_run(Ctx, Script).


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
