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


multi_stable_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    lists:foreach(fun(I) ->
        ?assertMatch({ok, _}, chakra:run(Ctx, <<"baz();">>))
    end, lists:seq(1, 100)).


run_after_exception_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script1 = <<"throw \"I am exceptional!\"">>,
    Script2 = <<"2;">>,
    ?assertMatch({exception, _}, chakra:run(Ctx, Script1)),
    ?assertEqual({ok, <<"2">>}, chakra:run(Ctx, Script2)).
