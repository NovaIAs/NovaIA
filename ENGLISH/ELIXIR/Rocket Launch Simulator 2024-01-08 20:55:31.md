```elixir
defmodule Rocket do
  @moduledoc """
  A module for simulating rocket launches.
  """

  # Define the fields of a Rocket struct
  defstruct [
    :name,
    :type,
    :payload,
    :destination,
    :launch_date,
    :launch_time,
    :countdown_started,
    :countdown_value
  ]

  # Function to create a new Rocket struct
  def new(name, type, payload, destination, launch_date, launch_time) do
    %Rocket{
      name: name,
      type: type,
      payload: payload,
      destination: destination,
      launch_date: launch_date,
      launch_time: launch_time,
      countdown_started: false,
      countdown_value: 10
    }
  end

  # Function to start the countdown for a Rocket
  def start_countdown(rocket) do
    # Check if the countdown has already started
    if rocket.countdown_started do
      {:error, "Countdown has already started"}
    else
      # Update the rocket's countdown_started field to true
      rocket = %{rocket | countdown_started: true}

      # Start a new Task to decrement the countdown value every second
      Task.start(fn -> decrement_countdown(rocket) end)

      # Return the updated rocket
      {:ok, rocket}
    end
  end

  # Function to decrement the countdown value of a Rocket
  defp decrement_countdown(rocket) do
    # Check if the countdown has reached 0
    if rocket.countdown_value == 0 do
      # Update the rocket's countdown_started field to false
      rocket = %{rocket | countdown_started: false}

      # Launch the rocket
      launch_rocket(rocket)
    else
      # Decrement the countdown value
      rocket = %{rocket | countdown_value: rocket.countdown_value - 1}

      # Schedule the next decrement in 1 second
      Task.schedule_in(1000, fn -> decrement_countdown(rocket) end)
    end
  end

  # Function to launch a Rocket
  def launch_rocket(rocket) do
    # Print a message to the console
    IO.puts("🚀 #{rocket.name} has launched!")
  end
end

# Create a new Rocket struct
rocket = Rocket.new("Falcon 9", "Heavy", "Starlink Satellites", "Low Earth Orbit", "2023-03-08", "15:00:00")

# Start the countdown for the rocket
{:ok, rocket} = Rocket.start_countdown(rocket)

# Wait for the rocket to launch
while rocket.countdown_started do
  :timer.sleep(1000)
end
```

This code simulates the launch of a rocket. It defines a `Rocket` struct with fields for the rocket's name, type, payload, destination, launch date, launch time, whether the countdown has started, and the current countdown value.

The `new` function creates a new `Rocket` struct with the given values.

The `start_countdown` function starts the countdown for the rocket. It checks if the countdown has already started, and if not, it updates the rocket's `countdown_started` field to true and starts a new Task to decrement the countdown value every second.

The `decrement_countdown` function decrements the countdown value of the rocket. It checks if the countdown has reached 0, and if not, it decrements the countdown value and schedules the next decrement in 1 second.

The `launch_rocket` function launches the rocket. It prints a message to the console indicating that the rocket has launched.

The main part of the code creates a new `Rocket` struct, starts the countdown for the rocket, and then waits for the rocket to launch. It does this by calling the `start_countdown` function and then using a `while` loop to check if the rocket's `countdown_started` field is true. If the field is true, it means that the countdown is still running, so the code sleeps for 1 second and then checks again. Once the countdown is finished, the `while` loop exits and the `launch_rocket` function is called to launch the rocket.