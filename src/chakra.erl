-module(chakra).

-compile(no_native).
-on_load(init/0).

-export([
    create_runtime/0,
    create_runtime/1
]).


create_runtime() ->
    create_runtime([]).


create_runtime(Options) ->
    nif_create_runtime(Options).


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
