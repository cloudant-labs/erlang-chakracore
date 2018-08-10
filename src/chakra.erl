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

    create_context/1,
    serialize/3,
    run/3,
    run/4,
    eval/3,
    eval/4,
    call/4,
    idle/2
]).


-type runtime() :: reference().
-type context() :: reference().
-type script() :: reference().

% Can be either an atom or binary using dotted notation
% or a list of atom/binaries instead of dots.
-type function_name() :: [atom() | binary() | [atom() | binary()]].

-type runtime_opt() :: [
    {memory_limit, integer()}
    | disable_background_work
    | allow_script_interrupt
    | enable_idle_processing
    | disable_native_code_generation
    | disable_eval
    | enable_experimental_features
].


-type run_opt() :: [
    {source_url, binary()}
].


-type js_val() ::
    true
    | false
    | null
    | undefined
    | number()
    | atom()
    | binary()
    | [js_val()]
    | {[{atom() | binary(), js_val()}]}.



-spec create_runtime() -> {ok, runtime()} | {error, atom()}.
create_runtime() ->
    create_runtime([]).


-spec create_runtime([runtime_opt()]) -> {ok, runtime()} | {error, atom()}.
create_runtime(Options) when is_list(Options) ->
    nif_create_runtime(Options).


-spec memory_usage(runtime()) -> {ok, non_neg_integer()} | {error, any()}.
memory_usage(Runtime) ->
    % Deliberately not using the async handler.
    nif_memory_usage(Runtime).


-spec gc(runtime()) -> ok | {error, any()}.
gc(Runtime) ->
    async(nif_gc(Runtime)).


-spec enable(runtime()) -> ok | {error, any()}.
enable(Runtime) ->
    async(nif_enable(Runtime)).


-spec disable(runtime()) -> ok | {error, any()}.
disable(Runtime) ->
    async(nif_disable(Runtime)).


-spec interrupt(runtime()) -> ok | {error, any()}.
interrupt(Runtime) ->
    % Deliberately not using the async handler.
    nif_interrupt(Runtime).


-spec create_context(runtime()) -> {ok, context()} | {error, atom()}.
create_context(Runtime) ->
    async(nif_create_context(Runtime)).


-spec serialize(runtime(), context(), binary()) ->
        {ok, script()} | {error, any()}.
serialize(Runtime, Ctx, Script) when is_binary(Script) ->
    async(nif_serialize(Runtime, Ctx, Script)).


-spec run(runtime(), context(), script()) ->
        {ok, js_val()} | {exception, any()} | {error, any()}.
run(Runtime, Ctx, Script) ->
    run(Runtime, Ctx, Script, []).


-spec run(runtime(), context(), script(), [run_opt()]) ->
        {ok, js_val()} | {exception, any()} | {error, any()}.
run(Runtime, Ctx, SerializedScript, Opts) ->
    async(nif_run(Runtime, Ctx, SerializedScript, Opts)).


-spec eval(runtime(), context(), binary()) ->
        {ok, js_val()} | {exception, any()} | {error, any()}.
eval(Runtime, Ctx, Script) when is_binary(Script) ->
    eval(Runtime, Ctx, Script, []).


-spec eval(runtime(), context(), binary(), [run_opt()]) ->
        {ok, js_val()} | {exception, any()} | {error, any()}.
eval(Runtime, Ctx, Script, Opts) when is_binary(Script), is_list(Opts) ->
    case serialize(Runtime, Ctx, Script) of
        {ok, Serialized} ->
            run(Runtime, Ctx, Serialized, Opts);
        Else ->
            Else
    end.


-spec call(runtime(), context(), function_name(), [js_val()]) ->
        {ok, any()} | {exception, any()} | {error, any()}.
call(Runtime, Ctx, Name, Args) when is_atom(Name) ->
    call(Runtime, Ctx, list_to_binary(atom_to_list(Name)), Args);

call(Runtime, Ctx, Name, Args) when is_binary(Name) ->
    call(Runtime, Ctx, binary:split(Name, <<".">>, [global]), Args);

call(Runtime, Ctx, Name, Args) when is_list(Name), is_list(Args) ->
    async(nif_call(Runtime, Ctx, Name, Args)).


-spec idle(runtime(), context()) -> ok | {error, any()}.
idle(Runtime, Ctx) ->
    async(nif_idle(Runtime, Ctx)).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    NumSchedulers = erlang:system_info(schedulers),
    erlang:load_nif(filename:join(PrivDir, "chakra"), NumSchedulers).


async({ok, Ref}) when is_reference(Ref) ->
    receive
        {Ref, Resp} ->
            Resp
    end;
async(Else) ->
    Else.


-define(NOT_LOADED, erlang:nif_error({chakra_nif_not_loaded, ?FILE, ?LINE})).

nif_create_runtime(_Options) -> ?NOT_LOADED.
nif_memory_usage(_Rt) -> ?NOT_LOADED.
nif_gc(_Rt) -> ?NOT_LOADED.
nif_enable(_Rt) -> ?NOT_LOADED.
nif_disable(_Rt) -> ?NOT_LOADED.
nif_interrupt(_Rt) -> ?NOT_LOADED.

nif_create_context(_Rt) -> ?NOT_LOADED.
nif_serialize(_Rt, _Ctx, _Script) -> ?NOT_LOADED.
nif_run(_Rt, _Ctx, _Script, _Opts) -> ?NOT_LOADED.
nif_call(_Rt, _Ctx, _Name, _Args) -> ?NOT_LOADED.
nif_idle(_Rt, _Ctx) -> ?NOT_LOADED.

