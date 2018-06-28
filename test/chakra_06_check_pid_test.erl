-module(chakra_06_check_pid_test).

-include_lib("eunit/include/eunit.hrl").


setup() ->
    Self = self(),
    Pid = spawn(fun() ->
        erlang:monitor(process, Self),
        {ok, Ctx} = chakra:create_context(),
        Script = <<"function ident(a) {return a;};">>,
        {ok, _} = chakra:run(Ctx, Script),
        receive
            {Self, get} ->
                Self ! {ok, Ctx}
        end,
        receive
            {'DOWN', _, _, _, _} -> ok
        end
    end),
    Pid ! {Self, get},
    receive
        {ok, Ctx} -> Ctx
    end.


run_test() ->
    Ctx = setup(),
    ?assertError(badarg, chakra:run(Ctx, <<"1;">>)).


call_test() ->
    Ctx = setup(),
    ?assertError(badarg, chakra:call(Ctx, ident, [true])).


gc_test() ->
    Ctx = setup(),
    ?assertError(badarg, chakra:gc(Ctx)).
