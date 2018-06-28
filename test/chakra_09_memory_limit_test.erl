-module(chakra_09_memory_limit_test).

-include_lib("eunit/include/eunit.hrl").


script() ->
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
    {ok, Ctx} = chakra:create_context(),
    ?assertMatch({ok, _}, chakra:run(Ctx, script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


explicit_no_memory_limit_test() ->
    {ok, Ctx} = chakra:create_context([{memory_limit, -1}]),
    ?assertMatch({ok, _}, chakra:run(Ctx, script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


memory_limit_test() ->
    {ok, Ctx} = chakra:create_context([{memory_limit, 50 * 1048576}]),
    ?assertMatch({ok, _}, chakra:run(Ctx, script())),
    ?assertEqual({ok, 100000}, chakra:call(Ctx, grow, [])).


out_of_memory_test() ->
    {ok, Ctx} = chakra:create_context([{memory_limit, 1048576}]),
    ?assertMatch({ok, _}, chakra:run(Ctx, script())),
    ?assertEqual({exception, out_of_memory}, chakra:call(Ctx, grow, [])).


