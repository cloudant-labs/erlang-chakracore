-module(chakra_04_conversion_test).

-include_lib("eunit/include/eunit.hrl").


js_to_erl_test_() ->
    Pairs = [
        {<<"undefined;">>, undefined},
        {<<"null;">>, null},
        {<<"true;">>, true},
        {<<"false;">>, false},
        {<<"1;">>, 1},
        {<<"3.14;">>, 3.14},
        {<<"\"a string\";">>, <<"a string">>},
        {
            <<"var a = {\"foo\": true, \"bar\": 2}; a;">>,
            {[{<<"foo">>, true}, {<<"bar">>, 2}]}
        },
        {
            <<"throw \"an error\"">>,
            <<"an error">> % Might be an object?
        },
        {
            <<"[1, true, \"stuff\"]">>,
            [1, true, <<"stuff">>]
        },
        {
            <<"function foo() {}; foo;">>,
            <<"function foo() {}">>
        }
    ],
    {ok, Ctx} = chakra:create_context(),
    lists:map(fun({Js, Expect}) ->
        {Js, ?_assertEqual(Expect, element(2, chakra:run(Ctx, Js)))}
    end, Pairs).


rt_test_() ->
    Items = [
        undefined,
        null,
        true,
        false,
        1,
        3.14,
        <<"a string">>,
        {[]},
        {[{<<"foo">>, true}, {<<"bar">>, [2, null]}]},
        [],
        [1, true, <<"stuff">>]
    ],
    {ok, Ctx} = chakra:create_context(),
    {ok, _} = chakra:run(Ctx, <<"function ident(a) {return a;};">>),
    lists:map(fun(Value) ->
        Name = lists:flatten(io_lib:format("~p", [Value])),
        {
            Name,
            ?_assertEqual(Value, element(2, chakra:call(Ctx, ident, [Value])))
        }
    end, Items).


bad_term_test_() ->
    {ok, Ctx} = chakra:create_context(),
    {ok, _} = chakra:run(Ctx, <<"function ident(a) {return a;};">>),
    [
        ?_assertMatch(
            {invalid_term, _},
            chakra:call(Ctx, ident, [erlang:make_ref()])
        ),
        ?_assertMatch(
            {invalid_term, _},
            chakra:call(Ctx, ident, [self()])
        ),
        ?_assertMatch(
            {invalid_number, _},
            chakra:call(Ctx, ident, [49872348792873492874983724])
        )
    ].


bad_list_test_() ->
    {ok, Ctx} = chakra:create_context(),
    {ok, _} = chakra:run(Ctx, <<"function ident(a) {return a;};">>),
    ?_assertMatch(
        {invalid_term, _},
        chakra:call(Ctx, ident, [[erlang:make_ref()]])
    ).


bad_object_test_() ->
    {ok, Ctx} = chakra:create_context(),
    {ok, _} = chakra:run(Ctx, <<"function ident(a) {return a;};">>),
    [
        ?_assertMatch(
            {invalid_term, _},
            chakra:call(Ctx, ident, [{a, random, tuple}])
        ),
        ?_assertMatch(
            {invalid_term, _},
            chakra:call(Ctx, ident, [{1}])
        ),
        ?_assertMatch(
            {invalid_property, _},
            chakra:call(Ctx, ident, [{[erlang:make_ref()]}])
        ),
        ?_assertMatch(
            {invalid_property, _},
            chakra:call(Ctx, ident, [{[{a, bad, property}]}])
        ),
        ?_assertMatch(
            {invalid_key, _},
            chakra:call(Ctx, ident, [{[{4, true}]}])
        ),
        ?_assertMatch(
            {invalid_key, _},
            chakra:call(Ctx, ident, [{[{erlang:make_ref(), true}]}])
        ),
        ?_assertMatch(
            {invalid_term, _},
            chakra:call(Ctx, ident, [{[{foo, erlang:make_ref()}]}])
        )
    ].
