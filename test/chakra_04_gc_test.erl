-module(chakra_04_gc_test).

-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertEqual(ok, chakra:gc(Ctx)).

small_gc_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<"function baz() {return \"Ohai!\";}">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    lists:foreach(fun(_) ->
        ?assertMatch({ok, _}, chakra:run(Ctx, <<"baz();">>))
    end, lists:seq(1, 100)),
    ?assertEqual(ok, chakra:gc(Ctx)).
