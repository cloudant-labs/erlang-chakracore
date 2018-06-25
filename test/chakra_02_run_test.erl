-module(chakra_02_run_test).

-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertEqual({ok, <<"foo">>}, chakra:run(Ctx, <<"\"foo\";">>)).


stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    ?assertEqual({ok, <<"Ohai!">>}, chakra:run(Ctx, <<"baz();">>)).