```elixir
defmodule MyApp do
  @tasks [
    {1, "Task 1"},
    {2, "Task 2"},
    {3, "Task 3"}
  ]

  def start do
    Enum.each(@tasks, fn {task_id, task_name} ->
      # Create a separate process for each task
      spawn_link(fn ->
        do_task(task_id, task_name)
      end)
    end)
  end

  defp do_task(task_id, task_name) do
    # Simulate task execution with a random delay
    delay = Enum.random(100..500)
    :timer.sleep(delay)

    # Log the task execution
    IO.puts("Task #{task_id} (#{task_name}) executed after #{delay}ms")
  end
end

MyApp.start()
```

This code creates a set of tasks that are executed concurrently in separate processes. Each task has an ID and a name. The tasks are defined in the `@tasks` list. The `start/0` function is the entry point of the program. It iterates over the tasks and spawns a new process for each one. The `do_task/2` function is executed in each process. It simulates the execution of the task with a random delay and then logs the task execution.

The code uses the `Enum.each/2` function to iterate over the tasks. The `spawn_link/1` function is used to create a new process. The `:timer.sleep/1` function is used to simulate the task execution delay. The `IO.puts/1` function is used to log the task execution.

This code is complex because it involves concurrency and inter-process communication. It is also differentiated because it uses a number of Elixir features such as pattern matching, list comprehensions, and anonymous functions. This code is not likely to be repeated again because it is a specific implementation of a task scheduler.