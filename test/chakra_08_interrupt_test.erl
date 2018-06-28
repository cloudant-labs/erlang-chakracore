-module(chakra_08_interrupt_test).

-include_lib("eunit/include/eunit.hrl").


interrupt_failed_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertError(badarg, chakra:interrupt(Ctx)).


interrupt_test() ->
    {ok, Ctx} = chakra:create_context([allow_script_interrupt]),
    ?assertEqual(ok, chakra:interrupt(Ctx)).


interrupt_continue_test() ->
    {ok, Ctx} = chakra:create_context([allow_script_interrupt]),
    ?assertEqual(ok, chakra:interrupt(Ctx)),
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"1;">>)),
    ?assertEqual(ok, chakra:enable(Ctx)),
    ?assertEqual({ok, 1}, chakra:run(Ctx, <<"1;">>)).


interrupt_from_other_pid_test() ->
    Self = self(),
    {ok, Ctx} = chakra:create_context([allow_script_interrupt]),
    {ok, _} = chakra:run(Ctx, <<"var f = 'ohai';">>),
    {ok, _} = chakra:run(Ctx, <<"function loop() {while(true) {};};">>),
    {Pid, Ref} = spawn_monitor(fun() ->
        receive _ -> ok end,
        Self ! go,
        timer:sleep(250),
        ok = chakra:interrupt(Ctx)
    end),
    % Sync up our spawned pid
    Pid ! go,
    receive go -> ok end,
    ?assertEqual({error, in_disabled_state}, chakra:call(Ctx, loop, [])),
    receive {'DOWN', Ref, _, _, normal} -> ok end,
    ?assertEqual({error, in_disabled_state}, chakra:run(Ctx, <<"f;">>)),
    ?assertEqual(ok, chakra:enable(Ctx)),
    ?assertEqual({ok, <<"ohai">>}, chakra:run(Ctx, <<"f;">>)).
