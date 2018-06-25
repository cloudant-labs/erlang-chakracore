-module(chakra_01_test).

-include_lib("eunit/include/eunit.hrl").


basic_test() ->
    ?assertError(badarg, chakra:create_runtime()).
