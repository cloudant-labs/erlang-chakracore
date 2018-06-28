-module(chakra_07_enable_disable_test).

-include_lib("eunit/include/eunit.hrl").


enable_disable_test() ->
    {ok, Ctx} = chakra:create_context([allow_script_interrupt]),

    ok = chakra:enable(Ctx),
    ok = chakra:disable(Ctx),
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"1;">>)),

    ok = chakra:disable(Ctx),
    ok = chakra:enable(Ctx),
    ?assertEqual({ok, 1}, chakra:run(Ctx, <<"1;">>)).
