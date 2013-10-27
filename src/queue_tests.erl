% Simple tests for queue module

-module(queue_tests).
-export([run_tests/0]).

test_enqueue() ->
    io:format("Running test to add 100 items ~n"),
    {ok, Pid} = queue:start_link(),
    E = lists:seq(1,1000),
    lists:foreach(fun(I) -> io:format("Adding itm ~p ~n", [I]), queue:enqueue(Pid, I) end, E),
    lists:foreach(fun(I) -> G = queue:dequeue_noblock(Pid), io:format("Received ~p = ~p ~n", [G,I]) end, E).

wait_and_enqueue(Pid) ->
    receive
    after 2000 ->
              queue:enqueue(Pid, "item"),
              io:format("Enqueued item ~p ~n", ["item"])
    end.

test_blockdequeue() ->
    io:format("Running test to check block dequeue ~n"),
    {ok, Pid} = queue:start_link(),
    spawn(fun() -> wait_and_enqueue(Pid) end),
    io:format("Block waiting ~n"), io:format("Received itm ~p ~n", [queue:dequeue(Pid)]).

run_tests() ->
    test_enqueue(),
    test_blockdequeue().

