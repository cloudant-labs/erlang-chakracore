% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

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
    serialize/2,
    run/2,
    run/3,
    eval/2,
    eval/3,
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


serialize(Ctx, Script) when is_binary(Script) ->
    nif_serialize(Ctx, Script).


run(Ctx, SerializedScript) ->
    run(Ctx, SerializedScript, []).


run(Ctx, SerializedScript, Opts) ->
    nif_run(Ctx, SerializedScript, Opts).


eval(Ctx, Script) when is_binary(Script) ->
    eval(Ctx, Script, []).


eval(Ctx, Script, Opts) when is_binary(Script), is_list(Opts) ->
    case serialize(Ctx, Script) of
        {ok, Serialized} ->
            run(Ctx, Serialized, Opts);
        Else ->
            Else
    end.


call(Ctx, Name, Args) when is_atom(Name) ->
    call(Ctx, list_to_binary(atom_to_list(Name)), Args);

call(Ctx, Name, Args) when is_binary(Name) ->
    call(Ctx, binary:split(Name, <<".">>, [global]), Args);

call(Ctx, Name, Args) when is_list(Name), is_list(Args) ->
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


-define(NOT_LOADED, erlang:nif_error({chakra_nif_not_loaded, ?FILE, ?LINE})).

nif_create_runtime(_Options) -> ?NOT_LOADED.
nif_memory_usage(_Ctx) -> ?NOT_LOADED.
nif_gc(_Ctx) -> ?NOT_LOADED.
nif_enable(_Ctx) -> ?NOT_LOADED.
nif_disable(_Ctx) -> ?NOT_LOADED.
nif_interrupt(_Ctx) -> ?NOT_LOADED.

nif_create_context(_Options) -> ?NOT_LOADED.
nif_serialize(_Ctx, _Script) -> ?NOT_LOADED.
nif_run(_Ctx, _SerializedScript, _Opts) -> ?NOT_LOADED.
nif_call(_Ctx, _Name, _Args) -> ?NOT_LOADED.
nif_idle(_Ctx) -> ?NOT_LOADED.

