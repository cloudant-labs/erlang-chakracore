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


idle_test() ->
    {ok, Ctx1} = chakra:create_context(),
    ?assertEqual({error,idle_not_enabled}, chakra:idle(Ctx1)),

    {ok, Ctx2} = chakra:create_context([enable_idle_processing]),
    {ok, N2} = chakra:idle(Ctx2),
    ?assert(is_integer(N2) andalso N2 >= 0).


opt_test() ->
    Options = [
        {memory_limit, 1048576},
        disable_background_work,
        allow_script_interrupt,
        enable_idle_processing,
        disable_native_code_generation,
        disable_eval,
        enable_experimental_features
    ],
    lists:foreach(fun(Opt) ->
        ?assertMatch({ok, _}, chakra:create_context([Opt]))
    end, Options).


bad_opt_test() ->
    ?assertError(badarg, chakra:create_context([{}])),
    ?assertError(badarg, chakra:create_context([{a, b, c}])),
    ?assertError(badarg, chakra:create_context([{foo, 0}])),
    ?assertError(badarg, chakra:create_context([{memory_limit, false}])),
    ?assertError(badarg, chakra:create_context([{memory_limit, -2}])),
    ?assertError(badarg, chakra:create_context([bad_arg_name])),
    ?assertError(badarg, chakra:create_context([disable_eval, bad_arg_name])).
