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
    ?assertEqual({ok, <<"foo">>}, chakra:run(Ctx, <<"\"foo\";">>)).


stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    ?assertEqual({ok, <<"Ohai!">>}, chakra:run(Ctx, <<"baz();">>)).


multi_stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    lists:foreach(fun(_I) ->
        ?assertMatch({ok, _}, chakra:run(Ctx, <<"baz();">>))
    end, lists:seq(1, 100)).


run_after_exception_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script1 = <<"throw \"I am exceptional!\"">>,
    Script2 = <<"2;">>,
    ?assertMatch({exception, _}, chakra:run(Ctx, Script1)),
    ?assertEqual({ok, 2}, chakra:run(Ctx, Script2)).


simple_call_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    ?assertEqual({ok, <<"Ohai!">>}, chakra:call(Ctx, baz, [])).


call_arg_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function dbl(val) {return val * 2;}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    ?assertEqual({ok, 2}, chakra:call(Ctx, dbl, [1])).


call_multi_arg_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function conc(a, b) {return a + \"-\" + b;}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    ?assertEqual(
            {ok, <<"foo-baz">>},
            chakra:call(Ctx, <<"conc">>, [foo, baz])
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
        ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
        Ctx
    end, lists:seq(1, NumContexts)),
    lists:foreach(fun({Ctx, I}) ->
        ?assertEqual({ok, I}, chakra:run(Ctx, <<"a;">>))
    end, lists:zip(Contexts, lists:seq(1, NumContexts))).
