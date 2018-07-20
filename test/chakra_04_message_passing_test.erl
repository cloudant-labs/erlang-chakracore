% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chakra_04_message_passing_test).

-include_lib("eunit/include/eunit.hrl").


simple_receive_test() ->
    {ok, Ctx} = chakra:create_context(),
    ok = chakra:send(Ctx, <<"ohai!">>),
    ?assertEqual({ok, <<"ohai!">>}, chakra:eval(Ctx, <<"erlang.receive();">>)).


receive_multiple_test() ->
    {ok, Ctx} = chakra:create_context(),
    lists:foreach(fun(I) ->
        ok = chakra:send(Ctx, I)
    end, lists:seq(1, 10)),
    Script = <<
        "var s = 0;\n"
        "while(erlang.has_message()) {\n"
        "    s += erlang.receive();\n"
        "}\n"
        "s;\n"
    >>,
    ?assertEqual({ok, 55}, chakra:eval(Ctx, Script)).


receive_from_remote_test() ->
    {ok, Ctx} = chakra:create_context(),
    spawn_link(fun() ->
        timer:sleep(500),
        ok = chakra:send(Ctx, <<"yay!">>)
    end),
    ?assertEqual({ok, <<"yay!">>}, chakra:eval(Ctx, <<"erlang.receive();">>)).


simple_send_test() ->
    {ok, Ctx} = chakra:create_context(),
    {ok, undefined} = chakra:call(Ctx, 'erlang.send', [self(), ohai]),
    receive <<"ohai">> -> ok end.


send_multiple_test() ->
    {ok, Ctx} = chakra:create_context(),
    Script = <<
        "var pid = null;\n"
        "function set_pid(p) {pid = p;};\n"
        "function go() {\n"
        "  for(var i = 1; i <= 10; i++) {\n"
        "    erlang.send(pid, i);\n"
        "  }\n"
        "};\n"
    >>,
    {ok, undefined} = chakra:eval(Ctx, Script),
    {ok, undefined} = chakra:call(Ctx, set_pid, [self()]),
    {ok, undefined} = chakra:call(Ctx, go, []),
    Total = lists:foldl(fun(I, Acc) ->
        Val = receive V -> V end,
        ?assertEqual(I, Val),
        Acc + Val
    end, 0, lists:seq(1, 10)),
    ?assertEqual(55, Total).


send_remote_test() ->
    Self = self(),
    Pid = spawn_link(fun() ->
        receive <<"foo">> -> ok end,
        Self ! got_foo
    end),
    {ok, Ctx} = chakra:create_context(),
    {ok, undefined} = chakra:call(Ctx, 'erlang.send', [Pid, foo]),
    receive got_foo -> ok end.


send_many_remote_test() ->
    Self = self(),
    Pid = spawn_link(fun() ->
        Total = lists:foldl(fun(I, Acc) ->
            Val = receive V -> V end,
            ?assertEqual(I, Val),
            Acc + Val
        end, 0, lists:seq(1, 5000)),
        Self ! {total, Total}
    end),
    {ok, Ctx} = chakra:create_context(),
    Script = <<
        "var pid = null;\n"
        "function set_pid(p) {pid = p;};\n"
        "function go() {\n"
        "  for(var i = 1; i <= 5000; i++) {\n"
        "    erlang.send(pid, i);\n"
        "  }\n"
        "};\n"
    >>,
    {ok, undefined} = chakra:eval(Ctx, Script),
    {ok, undefined} = chakra:call(Ctx, set_pid, [Pid]),
    {ok, undefined} = chakra:call(Ctx, go, []),
    Total = receive {total, T} -> T end,
    ?assertEqual(5000 * 5001 div 2, Total).


send_missing_args_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertMatch({exception, _}, chakra:call(Ctx, 'erlang.send', [])),
    ?assertMatch({exception, _}, chakra:call(Ctx, 'erlang.send', [self()])).


send_extra_args_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertMatch(
            {exception, _},
            chakra:call(Ctx, 'erlang.send', [self(), foo, bar])
        ).


send_bad_pid_test() ->
    {ok, Ctx} = chakra:create_context(),
    ?assertMatch({exception, _}, chakra:call(Ctx, 'erlang.send', [foo, bar])).


% send_bad_message_test - Need to figure out how to check this
