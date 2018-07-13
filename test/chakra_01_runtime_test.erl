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

-module(chakra_01_runtime_test).

-include_lib("eunit/include/eunit.hrl").


create_runtime_test() ->
    Resp = chakra:create_runtime(),
    ?assertMatch({ok, _}, Resp),
    ?assert(is_reference(element(2, Resp))).


opt_test() ->
    Options = [
        {memory_limit, 1048576},
        disable_background_work,
        allow_script_interrupt,
        enable_idle_processing,
        disable_native_code_generation,
        disable_eval,
        enable_experimental_features
    ],
    lists:foreach(fun(Opt) ->
        ?assertMatch({ok, _}, chakra:create_runtime([Opt]))
    end, Options).


bad_opt_test() ->
    ?assertError(function_clause, chakra:create_runtime(foo)),
    ?assertError(badarg, chakra:create_runtime([{}])),
    ?assertError(badarg, chakra:create_runtime([{a, b, c}])),
    ?assertError(badarg, chakra:create_runtime([{foo, 0}])),
    ?assertError(badarg, chakra:create_runtime([{memory_limit, false}])),
    ?assertError(badarg, chakra:create_runtime([{memory_limit, -2}])),
    ?assertError(badarg, chakra:create_runtime([bad_arg_name])),
    ?assertError(badarg, chakra:create_runtime([disable_eval, bad_arg_name])).


memory_usage_test() ->
    {ok, Rt} = chakra:create_runtime(),
    {ok, Ctx} = chakra:create_context(Rt),

    {ok, Size1} = chakra:memory_usage(Rt),
    ?assert(is_integer(Size1) andalso Size1 >= 0),

    Script = <<"var a = []; for(i = 0; i < 100000; i++) {a.push(i);};">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),

    {ok, Size2} = chakra:memory_usage(Rt),
    ?assert(is_integer(Size2) andalso Size2 >= Size1).


simple_gc_test() ->
    {ok, Rt} = chakra:create_runtime(),
    ?assertEqual(ok, chakra:gc(Rt)).


small_gc_test() ->
    {ok, Rt} = chakra:create_runtime(),
    {ok, Ctx} = chakra:create_context(Rt),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    lists:foreach(fun(_) ->
        ?assertMatch({ok, _}, chakra:run(Ctx, <<"baz();">>))
    end, lists:seq(1, 100)),
    ?assertEqual(ok, chakra:gc(Rt)).


memory_drop_test() ->
    {ok, Rt} = chakra:create_runtime(),
    {ok, Ctx} = chakra:create_context(Rt),
    {ok, Size1} = chakra:memory_usage(Rt),
    ?assert(is_integer(Size1) andalso Size1 >= 0),

    Script1 = <<"var a = []; for(i = 0; i < 100000; i++) {a.push(i);};">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script1)),
    {ok, Size2} = chakra:memory_usage(Rt),
    ?assert(is_integer(Size2) andalso Size2 >= Size1),

    Script2 = <<"a = undefined;">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script2)),
    ?assertEqual(ok, chakra:gc(Rt)),
    {ok, Size3} = chakra:memory_usage(Rt),
    ?assert(is_integer(Size3) andalso Size3 < Size2).


ctx_gc_test() ->
    {ok, Rt} = chakra:create_runtime([disable_background_work]),
    {ok, Size1} = chakra:memory_usage(Rt),

    create_destroy_contexts(Rt),
    {ok, Size2} = chakra:memory_usage(Rt),

    erlang:garbage_collect(),
    chakra:gc(Rt),

    {ok, Size3} = chakra:memory_usage(Rt),

    ?assert(Size1 < Size2 / 2),
    ?assert(Size3 < Size2 / 2).


create_destroy_contexts(Rt) ->
    NumContexts = 100,
    Contexts = lists:map(fun(I) ->
        {ok, Ctx} = chakra:create_context(Rt),
        ?assertMatch({ok, _}, chakra:run(Ctx, mem_limit_script())),
        ?assertMatch({ok, _}, chakra:call(Ctx, grow, [])),
        Ctx
    end, lists:seq(1, NumContexts)).


%% TODO: Still not convinced by this test. I'm trying
%% to figure out of JSRT will keep a reference to our
%% script that's used by the context at some unspecified
%% point in the future which means we'd need to incref
%% all scripts that are run in the context until the context
%% is destroyed.
gc_script_test() ->
    erlang:garbage_collect(self()),
    {binary, Before} = process_info(self(), binary),

    {ok, Ctx} = chakra:create_context(),
    {binary, With} = run_gc_script(Ctx, rand:uniform(5)),

    ?assertNotEqual(Before, With),

    erlang:garbage_collect(self()),
    {binary, After} = process_info(self(), binary),
    ?assertEqual(Before, After),

    ?assertMatch({ok, _}, chakra:call(Ctx, b, [])).


run_gc_script(Ctx, Val) ->
    Padding = lists:duplicate(Val, " "),
    ScriptStr = "function a() {return true;}; function b() {return a.toString()};",
    Script = iolist_to_binary(Padding ++ ScriptStr),
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    process_info(self(), binary).


mem_limit_script() ->
    <<
        "function grow() {\n"
        "  var array = [];\n"
        "  for(i = 0; i < 100000; i++) {\n"
        "    array.push(i);\n"
        "  }\n"
        "  return array.length;\n"
        "};"
    >>.


no_memory_limit_test() ->
    {ok, Rt} = chakra:create_runtime(),
    {ok, Ctx} = chakra:create_context(Rt),
    ?assertMatch({ok, _}, chakra:run(Ctx, mem_limit_script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


explicit_no_memory_limit_test() ->
    {ok, Rt} = chakra:create_runtime([{memory_limit, -1}]),
    {ok, Ctx} = chakra:create_context(Rt),
    ?assertMatch({ok, _}, chakra:run(Ctx, mem_limit_script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


memory_limit_test() ->
    {ok, Rt} = chakra:create_runtime([{memory_limit, 50 * 1048576}]),
    {ok, Ctx} = chakra:create_context(Rt),
    ?assertMatch({ok, _}, chakra:run(Ctx, mem_limit_script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


out_of_memory_test() ->
    {ok, Rt} = chakra:create_runtime([{memory_limit, 1048576}]),
    {ok, Ctx} = chakra:create_context(Rt),
    ?assertMatch({ok, _}, chakra:run(Ctx, mem_limit_script())),
    ?assertEqual({exception, out_of_memory}, chakra:call(Ctx, grow, [])).


enable_disable_test() ->
    {ok, Rt} = chakra:create_runtime([allow_script_interrupt]),
    {ok, Ctx} = chakra:create_context(Rt),

    ok = chakra:enable(Rt),
    ok = chakra:disable(Rt),
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"1;">>)),

    ok = chakra:disable(Rt),
    ok = chakra:enable(Rt),
    ?assertEqual({ok, 1}, chakra:run(Ctx, <<"1;">>)).


interrupt_failed_test() ->
    {ok, Rt} = chakra:create_runtime(),
    ?assertError(badarg, chakra:interrupt(Rt)).


interrupt_test() ->
    {ok, Rt} = chakra:create_runtime([allow_script_interrupt]),
    ?assertEqual(ok, chakra:interrupt(Rt)).


interrupt_continue_test() ->
    {ok, Rt} = chakra:create_runtime([allow_script_interrupt]),
    {ok, Ctx} = chakra:create_context(Rt),

    ?assertEqual(ok, chakra:interrupt(Rt)),
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"1;">>)),

    ?assertEqual(ok, chakra:enable(Rt)),
    ?assertEqual({ok, 1}, chakra:run(Ctx, <<"1;">>)).


interrupt_from_other_pid_test() ->
    Self = self(),
    {ok, Rt} = chakra:create_runtime([allow_script_interrupt]),
    {ok, Ctx} = chakra:create_context(Rt),

    {ok, _} = chakra:run(Ctx, <<"var f = 'ohai';">>),
    {ok, _} = chakra:run(Ctx, <<"function loop() {while(true) {};};">>),
    {Pid, Ref} = spawn_monitor(fun() ->
        receive _ -> ok end,
        Self ! go,
        timer:sleep(250),
        ok = chakra:interrupt(Rt)
    end),
    % Sync up our spawned pid
    Pid ! go,

    receive go -> ok end,
    ?assertEqual({error, in_disabled_state}, chakra:call(Ctx, loop, [])),

    receive {'DOWN', Ref, _, _, normal} -> ok end,
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"f;">>)),

    ?assertEqual(ok, chakra:enable(Rt)),
    ?assertEqual({ok, <<"ohai">>}, chakra:run(Ctx, <<"f;">>)).


pid_check_setup() ->
    Self = self(),
    Pid = spawn(fun() ->
        erlang:monitor(process, Self),
        {ok, Rt} = chakra:create_runtime(),
        {ok, Ctx} = chakra:create_context(Rt),
        Script = <<"function ident(a) {return a;};">>,
        {ok, _} = chakra:run(Ctx, Script),
        receive
            {Self, get} ->
                Self ! {ok, Rt, Ctx}
        end,
        receive
            {'DOWN', _, _, _, _} -> ok
        end
    end),
    Pid ! {Self, get},
    receive
        {ok, Rt, Ctx} -> {Rt, Ctx}
    end.


run_test() ->
    {_Rt, Ctx} = pid_check_setup(),
    ?assertError(badarg, chakra:run(Ctx, <<"1;">>)).


call_test() ->
    {_Rt, Ctx} = pid_check_setup(),
    ?assertError(badarg, chakra:call(Ctx, ident, [true])).


gc_test() ->
    {Rt, _Ctx} = pid_check_setup(),
    ?assertError(badarg, chakra:gc(Rt)).
