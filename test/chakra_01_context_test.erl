-module(chakra_01_context_test).

-include_lib("eunit/include/eunit.hrl").


create_test() ->
    Resp = chakra:create_context(),
    ?assertMatch({ok, _}, Resp),
    ?assert(is_reference(element(2, Resp))).


badarg_test() ->
    ?assertError(badarg, chakra:create_context(foo)),
    ?assertError(badarg, chakra:create_context(1)),
    ?assertError(badarg, chakra:create_context(<<"stuff">>)).
