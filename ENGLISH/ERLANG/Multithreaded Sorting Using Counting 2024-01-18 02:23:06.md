```erlang

-module(multithreaded_sort_counting).

-export([start/0]).

start() ->
    %% Create a list of integers to sort
    Integers = lists:seq(1, 1000000),

    %% Create a pool of 4 worker processes
    Workers = lists:seq(1, 4),
    [spawn_worker(Worker) || Worker <- Workers],

    %% Create a channel to receive sorted results from the workers
    ResultsChannel = erlang:register(results_channel, erlang:self()),

    %% Send the list of integers to the workers for sorting
    [send_work(Worker, Integers, ResultsChannel) || Worker <- Workers],

    %% Wait for all the workers to finish sorting
    lists:foreach(fun (_) -> receive _ -> ok end end, Workers),

    %% Collect the sorted results from the channel
    SortedResults = collect_results(ResultsChannel),

    %% Print the sorted results
    io:format("Sorted results: ~p~n", [SortedResults]).

spawn_worker(Worker) ->
    spawn_link(fun () -> worker_process(Worker) end).

worker_process(Worker) ->
    receive
        {Integers, ResultsChannel} ->
            SortedIntegers = lists:sort(Integers),
            erlang:send(ResultsChannel, {Worker, SortedIntegers}),
            worker_process(Worker)
    end.

send_work(Worker, Integers, ResultsChannel) ->
    erlang:send(Worker, {Integers, ResultsChannel}).

collect_results(ResultsChannel) ->
    lists:reverse(collect_results(ResultsChannel, [])).

collect_results(_, Results) when length(Results) >= 4 ->
    Results;
collect_results(ResultsChannel, Results) ->
    receive
        {Worker, SortedIntegers} ->
            collect_results(ResultsChannel, [SortedIntegers | Results])
    end.

```

Explanation:

1. The `start/0` function is the entry point of the program. It creates a list of integers to sort, a pool of 4 worker processes, a channel to receive sorted results from the workers, and sends the list of integers to the workers for sorting.

2. The `spawn_worker/1` function is used to create a worker process. It creates a new process that will execute the `worker_process/1` function.

3. The `worker_process/1` function is the main function of the worker process. It receives work from the main process, sorts the integers, and sends the sorted results back to the main process through the channel.

4. The `send_work/3` function is used to send work to a worker process. It sends the list of integers to be sorted and the channel to which the sorted results should be sent.

5. The `collect_results/1` function is used to collect the sorted results from the workers. It waits for messages from the workers, collects the sorted results, and returns them as a list.

6. The `collect_results/2` function is a helper function that is used to collect the sorted results from the workers. It recursively calls itself until it has collected all the results.

7. The `lists:seq/2` function is used to create a list of integers from a starting and ending value.

8. The `lists:sort/1` function is used to sort a list of integers in ascending order.

9. The `erlang:send/2` function is used to send a message to a process.

10. The `erlang:receive/0` function is used to receive a message from a process.