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

-module(chakra_02_context_test).

-include_lib("eunit/include/eunit.hrl").


idle_test() ->
    {ok, Ctx1} = chakra:create_context(),
    ?assertEqual({error,idle_not_enabled}, chakra:idle(Ctx1)),

    {ok, Ctx2} = chakra:create_context([enable_idle_processing]),
    {ok, N2} = chakra:idle(Ctx2),
    ?assert(is_integer(N2) andalso N2 >= 0).


simple_run_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertEqual({ok, <<"foo">>}, chakra:eval(Ctx, <<"\"foo\";">>)).


stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    ?assertEqual({ok, <<"Ohai!">>}, chakra:eval(Ctx, <<"baz();">>)).


multi_stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    lists:foreach(fun(_I) ->
        ?assertMatch({ok, _}, chakra:eval(Ctx, <<"baz();">>))
    end, lists:seq(1, 100)).


run_after_exception_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script1 = <<"throw \"I am exceptional!\"">>,
    Script2 = <<"2;">>,
    ?assertMatch({exception, _}, chakra:eval(Ctx, Script1)),
    ?assertEqual({ok, 2}, chakra:eval(Ctx, Script2)).


simple_call_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    ?assertEqual({ok, <<"Ohai!">>}, chakra:call(Ctx, baz, [])).


call_arg_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function dbl(val) {return val * 2;}">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    ?assertEqual({ok, 2}, chakra:call(Ctx, dbl, [1])).


call_multi_arg_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function conc(a, b) {return a + \"-\" + b;}">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    ?assertEqual(
            {ok, <<"foo-baz">>},
            chakra:call(Ctx, <<"conc">>, [foo, baz])
        ).


call_nested_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"var a = {\"b\":{\"c\":function conc(a, b) {return a + b;}}};">>,
    ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
    ?assertEqual(
            {ok, <<"foobaz">>},
            chakra:call(Ctx, 'a.b.c', [foo, baz])
        ),
    ?assertEqual(
            {ok, <<"foobaz">>},
            chakra:call(Ctx, [a, b, c], [foo, baz])
        ),
    ?assertEqual(
            {ok, <<"foobaz">>},
            chakra:call(Ctx, <<"a.b.c">>, [foo, baz])
        ),
    ?assertEqual(
            {ok, <<"foobaz">>},
            chakra:call(Ctx, [<<"a">>, <<"b">>, <<"c">>], [foo, baz])
        ),
    ?assertEqual(
            {ok, <<"foobaz">>},
            chakra:call(Ctx, [a, <<"b">>, c], [foo, baz])
        ).


undefined_function_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertEqual({error, undefined_function}, chakra:call(Ctx, foo, [bar])).


multi_ctx_test() ->
    NumContexts = 1000,
    {ok, Rt} = chakra:create_runtime(),
    Contexts = lists:map(fun(I) ->
        {ok, Ctx} = chakra:create_context(Rt),
        ScriptStr = io_lib:format("var a = ~b;~n", [I]),
        Script = iolist_to_binary(ScriptStr),
        ?assertMatch({ok, _}, chakra:eval(Ctx, Script)),
        Ctx
    end, lists:seq(1, NumContexts)),
    lists:foreach(fun({Ctx, I}) ->
        ?assertEqual({ok, I}, chakra:eval(Ctx, <<"a;">>))
    end, lists:zip(Contexts, lists:seq(1, NumContexts))).


serialize_test() ->
    Script = <<"function a() {return true;}; a.toString();">>,
    {ok, Ctx} = chakra:create_context(),
    ?assertMatch({ok, _}, chakra:serialize(Ctx, Script)).


run_serialized_test() ->
    Script = <<"function a() {return true;}; a.toString();">>,
    {ok, Ctx} = chakra:create_context(),
    {ok, Serialized} = chakra:serialize(Ctx, Script),
    ?assertMatch({ok, _}, chakra:run(Ctx, Serialized)).


run_serialized_on_different_runtime_test() ->
    Script = <<"function a() {return true;}; a.toString();">>,
    {ok, Rt1} = chakra:create_runtime(),
    {ok, Ctx1} = chakra:create_context(Rt1),
    {ok, Serialized} = chakra:serialize(Ctx1, Script),

    ?assertMatch({ok, _}, chakra:run(Ctx1, Serialized)),

    {ok, Rt2} = chakra:create_runtime(),
    {ok, Ctx2} = chakra:create_context(Rt2),
    ?assertMatch({ok, _}, chakra:run(Ctx2, Serialized)).


erlang_gc_serialized_test_() ->
    {timeout, 300, fun() ->
        % We're testing that the NIF resource is GC'ed here
        Script = <<"function a() {return true;}; a.toString();">>,
        {ok, Rt} = chakra:create_runtime(),
        {ok, Ctx} = chakra:create_context(Rt),

        {memory, Size1} = process_info(self(), memory),

        LoadFun = fun() ->
            All = lists:map(fun(_) ->
                {ok, _} = chakra:serialize(Ctx, Script)
            end, lists:seq(1, 10000)),
            process_info(self(), memory)
        end,

        {memory, Size2} = LoadFun(),
        erlang:garbage_collect(),
        {memory, Size3} = process_info(self(), memory),

        ?assert(Size1 < Size2),
        ?assert(Size3 < Size2 / 2)
    end}.


js_gc_serialized_test_() ->
    {timeout, 300, fun() ->
        % This test is covering that the ObjectBeforeCollectCallback
        % is called

        Script = <<"function a() {return true;}; a.toString();">>,
        {ok, Rt} = chakra:create_runtime([disable_background_work]),

        LoadFun = fun() ->
            Things = lists:map(fun(_) ->
                {ok, Ctx} = chakra:create_context(Rt),
                {ok, Serialized} = chakra:serialize(Ctx, Script),
                {ok, _} = chakra:run(Ctx, Serialized),
                {Ctx, Serialized}
            end, lists:seq(1, 1000)),
            {memory, ErlS2} = process_info(self(), memory),
            {ok, JsS2} = chakra:memory_usage(Rt),
            {ErlS2, JsS2}
        end,

        {memory, ErlSize1} = process_info(self(), memory),
        {ok, JsSize1} = chakra:memory_usage(Rt),

        {ErlSize2, JsSize2} = LoadFun(),

        erlang:garbage_collect(),
        ok = chakra:gc(Rt),

        {memory, ErlSize3} = process_info(self(), memory),
        {ok, JsSize3} = chakra:memory_usage(Rt),

        ?assert(ErlSize1 < ErlSize2 / 2),
        ?assert(ErlSize3 < ErlSize2 / 2),

        ?assert(JsSize1 < JsSize2 / 2),
        ?assert(JsSize3 < JsSize2 / 2)
    end}.


default_source_url_test() ->
    Script = <<"function a() {throw new Error();}; a();">>,
    {ok, Ctx} = chakra:create_context(),

    {exception, {ExcProps1}} = chakra:eval(Ctx, Script),
    {_, Stack1} = lists:keyfind(<<"stack">>, 1, ExcProps1),
    ?assertNotEqual(nomatch, binary:match(Stack1, <<"<ERLANG>">>)),

    {exception, {ExcProps2}} = chakra:call(Ctx, a, []),
    {_, Stack2} = lists:keyfind(<<"stack">>, 1, ExcProps2),
    ?assertNotEqual(nomatch, binary:match(Stack2, <<"<ERLANG>">>)).


source_url_test() ->
    Name = <<"myscript.js">>,
    Script = <<"function a() {throw new Error();}; a();">>,
    {ok, Ctx} = chakra:create_context(),

    {exception, {ExcProps1}} = chakra:eval(Ctx, Script, [{source_url, Name}]),
    {_, Stack1} = lists:keyfind(<<"stack">>, 1, ExcProps1),
    ?assertNotEqual(nomatch, binary:match(Stack1, Name)),

    {exception, {ExcProps2}} = chakra:call(Ctx, a, []),
    {_, Stack2} = lists:keyfind(<<"stack">>, 1, ExcProps2),
    ?assertNotEqual(nomatch, binary:match(Stack2, Name)).

