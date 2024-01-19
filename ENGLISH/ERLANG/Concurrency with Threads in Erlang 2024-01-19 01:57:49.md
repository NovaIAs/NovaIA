```erlang
-module(complex_erlang_code).

-export([main/0]).

main() ->
    spawn(
        fun() -> thread_function(1) end
    ),
    spawn(
        fun() -> thread_function(2) end
    ),
    io:format("Main process exiting~n").

thread_function(ThreadIndex) ->
    Ms = rand:uniform(250),
    timer:sleep(Ms),
    io:format("Thread ~p slept for ~p milliseconds~n", [ThreadIndex, Ms]),
    thread_function(ThreadIndex).
```

This Erlang code creates two separate threads, each of which prints out a random time interval that it slept for, and then calls itself recursively to continue the process. Here's how the code works:

1. **Module and Function Declaration**:

   ```erlang
   -module(complex_erlang_code).

   -export([main/0]).
   ```

   This declares an Erlang module named `complex_erlang_code` and exports a function named `main/0` from it. The `-export` directive makes the `main/0` function accessible to other modules and processes.

2. **`main/0` Function**:

   ```erlang
   main() ->
       spawn(
           fun() -> thread_function(1) end
       ),
       spawn(
           fun() -> thread_function(2) end
       ),
       io:format("Main process exiting~n").
   ```

   The `main/0` function is the entry point of the Erlang program. It performs the following tasks:

   - It uses the `spawn` function to create two separate threads.
   - Each thread is created by passing an anonymous function to the `spawn` function. This anonymous function is defined using the `fun` keyword and takes one argument, `ThreadIndex`.
   - The first thread is started with a call to `thread_function(1)`, and the second thread is started with a call to `thread_function(2)`.
   - After creating the threads, the main process prints a message indicating that it is exiting.

3. **`thread_function/1` Function**:

   ```erlang
   thread_function(ThreadIndex) ->
       Ms = rand:uniform(250),
       timer:sleep(Ms),
       io:format("Thread ~p slept for ~p milliseconds~n", [ThreadIndex, Ms]),
       thread_function(ThreadIndex).
   ```

   The `thread_function/1` function is the code that each thread executes. It does the following:

   - It generates a random time interval using the `rand:uniform/1` function, which generates a random integer between 1 and 250.
   - It uses the `timer:sleep/1` function to sleep for the generated time interval.
   - It prints a message indicating the thread number and the time interval it slept for.
   - Finally, it calls itself recursively to continue the process. This means that the thread continues sleeping for random intervals and printing messages until it is terminated.

When you run this code, the main process creates two threads, and each thread starts sleeping for random intervals and printing messages. The main process then exits, leaving the threads to continue running. You can observe the output of the threads in the console, which shows the thread number and the time interval each thread slept for.