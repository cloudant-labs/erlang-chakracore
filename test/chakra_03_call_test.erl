-module(chakra_03_call_test).

-include_lib("eunit/include/eunit.hrl").


simple_test() ->
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
