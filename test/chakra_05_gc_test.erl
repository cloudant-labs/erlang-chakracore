-module(chakra_05_gc_test).

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


memory_drop_test() ->
    {ok, Ctx} = chakra:create_context(),
    {ok, Size1} = chakra:memory_usage(Ctx),
    ?assert(is_integer(Size1) andalso Size1 >= 0),

    Script1 = <<"var a = []; for(i = 0; i < 100000; i++) {a.push(i);};">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script1)),
    {ok, Size2} = chakra:memory_usage(Ctx),
    ?assert(is_integer(Size2) andalso Size2 >= Size1),

    Script2 = <<"a = undefined;">>,
    ?assertMatch({ok, _}, chakra:run(Ctx, Script2)),
    ?assertEqual(ok, chakra:gc(Ctx)),
    {ok, Size3} = chakra:memory_usage(Ctx),
    ?assert(is_integer(Size3) andalso Size3 < Size2).


%% TODO: Still not convinced by this test. I'm trying
%% to figure out of JSRT will keep a reference to our
%% script that's used by the context at some unspecified
%% point in the future which means we'd need to incref
%% all scripts that are run in the context until the context
%% is destroyed.
gc_script_test() ->
    erlang:garbage_collect(self()),
    {binary, Before} = process_info(self(), binary),

    {ok, Ctx} = chakra:create_context(),
    {binary, With} = run_script(Ctx, rand:uniform(5)),

    ?assertNotEqual(Before, With),

    erlang:garbage_collect(self()),
    {binary, After} = process_info(self(), binary),
    ?assertEqual(Before, After),

    ?assertMatch({ok, _}, chakra:call(Ctx, b, [])).


run_script(Ctx, Val) ->
    Padding = lists:duplicate(Val, " "),
    ScriptStr = "function a() {return true;}; function b() {return a.toString()};",
    Script = iolist_to_binary(Padding ++ ScriptStr),
    ?assertMatch({ok, _}, chakra:run(Ctx, Script)),
    process_info(self(), binary).

