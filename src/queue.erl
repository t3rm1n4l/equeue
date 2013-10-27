% An unbounded queue with blocking and non-blocking dequeue operations

-module(queue).
-behavior(gen_server).

-export([start_link/0, enqueue/2, dequeue/1, dequeue_noblock/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Public API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

enqueue(Pid, Msg) ->
    gen_server:call(Pid, {message, Msg}).

dequeue(Pid) ->
    gen_server:call(Pid, {get, block}).

dequeue_noblock(Pid) ->
    gen_server:call(Pid, {get, noblock}).

% Server methods
init([]) ->
    {ok, {[], []}}.

handle_call({message, Msg}, _From, {[], [W|Rest]}) ->
    gen_server:reply(W, Msg),
    {reply, done, {[], Rest}};

handle_call({message, Msg}, _From, {Queue, Waiters}) ->
    {reply, done, {Queue ++ [Msg], Waiters}};

handle_call({get, _}, _, {[M|Rest], []}) ->
    {reply, M, {Rest, []}};

handle_call({get, noblock}, _, {[], _} = State) ->
    {reply, empty, State};

handle_call({get, block}, From, {[], W}) ->
    {noreply, {[], [From|W]}}.

handle_cast(_,State) ->
    {reply, State}.

handle_info(M, State) ->
    io:format("Invalid message : ~p ~n", [M]),
    {noreply, State}.

terminate(normal, _) ->
    io:format("Stopping elevator, state ~n", []),
    ok.

code_change(_, State, _) ->
    {ok, State}.

