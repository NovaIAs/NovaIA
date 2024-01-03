Certainly! Here's a complex and unique code in Elixir that showcases various features of the language:

```elixir
defmodule ComplexCode do
  require Logger

  @registry :complex_registry

  def start_link do
    Logger.info("Starting complex code...")
    {:ok, _pid} = Task.start_link(fn ->
      registry_loop()
    end)
  end

  defp registry_loop do
    registry = :ets.new(@registry, [:named_table, read_concurrency: true])
    Logger.info("Registry loop started.")

    loop(registry)
  end

  defp loop(registry) do
    receive do
      {:register, name, pid} ->
        :ets.insert(registry, {name, pid})
        Logger.info("Registered process #{inspect pid} with name #{inspect name}")

      {:unregister, name} ->
        :ets.delete(registry, name)
        Logger.info("Unregistered process with name #{inspect name}")

      {:send_to, name, message} ->
        case :ets.lookup(registry, name) do
          [{^name, pid}] ->
            send(pid, message)
            Logger.info("Sent message #{inspect message} to process #{inspect pid} with name #{inspect name}")
          _ ->
            Logger.error("Process with name #{inspect name} not found.")
        end

      :stop ->
        :ets.delete_all_objects(registry)
        Logger.info("Registry loop stopped.")
        :ok

      _ ->
        Logger.warn("Unhandled message.")
        loop(registry)
    end

    loop(registry)
  end
end

# Usage example
ComplexCode.start_link()

# Registering processes
Process.register(:process1, spawn(fn -> receive do _ -> IO.puts("Process 1 received a message.") end end))
Process.register(:process2, spawn(fn -> receive do _ -> IO.puts("Process 2 received a message.") end end))

# Sending messages
ComplexCode.send_to(:process1, "Hello from ComplexCode!")
ComplexCode.send_to(:process2, "Greetings from ComplexCode!")
```

This code defines a module called `ComplexCode` that implements a basic registry system using an ETS table. The registry is responsible for mapping names to process IDs (PIDs) so that messages can be sent to specific processes.

The `start_link` function starts a background task that runs the `registry_loop`. The `registry_loop` creates an ETS table and enters a loop where it waits for messages. It handles four types of messages:

- `{:register, name, pid}`: This message registers a process with a given name in the registry. It uses an ETS table to store the name-PID mappings and logs the registration.
- `{:unregister, name}`: This message removes a process from the registry based on its name. It deletes the corresponding mapping in the ETS table and logs the unregistration.
- `{:send_to, name, message}`: This message sends a message to a process with a specific name. It looks up the process PID in the ETS table and sends the message. If the process is not found, it logs an error.
- `:stop`: This message stops the registry loop. It deletes all objects in the ETS table and stops the loop.

The code also includes an example usage section. It demonstrates how to register processes using the `Process.register` function and how to send messages using the `ComplexCode.send_to` function.

Please note that this code is just an example to showcase various Elixir features. In a real-world scenario, you might want to use Elixir's built-in process registry (`Registry` module) or other mechanisms for process registration and communication.