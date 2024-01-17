```lua
-- This Lua script implements a simple finite-state machine (FSM) for controlling a robot.

-- Define the states of the FSM.
local STATES = {
  "IDLE",
  "MOVING",
  "TURNING",
  "STOPPING"
}

-- Define the transitions between the states.
local TRANSITIONS = {
  {from = "IDLE", to = "MOVING", event = "start_moving"},
  {from = "MOVING", to = "TURNING", event = "start_turning"},
  {from = "TURNING", to = "MOVING", event = "stop_turning"},
  {from = "MOVING", to = "STOPPING", event = "stop_moving"},
  {from = "TURNING", to = "STOPPING", event = "stop_turning"},
  {from = "STOPPING", to = "IDLE", event = "stop_stopping"}
}

-- Define the initial state of the FSM.
local current_state = "IDLE"

-- Define the event handler function.
local function handle_event(event)
  -- Find the transition that matches the current state and event.
  for i, transition in pairs(TRANSITIONS) do
    if transition.from == current_state and transition.event == event then
      -- Update the current state.
      current_state = transition.to

      -- Execute the transition's action.
      if transition.action then
        transition.action()
      end

      break
    end
  end
end

-- Define the action functions for the transitions.
local function start_moving()
  print("Starting to move")
end

local function stop_moving()
  print("Stopping to move")
end

local function start_turning()
  print("Starting to turn")
end

local function stop_turning()
  print("Stopping to turn")
end

local function stop_stopping()
  print("Stopping to stop")
end

-- Run the FSM.
while true do
  -- Wait for an event.
  local event = io.read()

  -- Handle the event.
  handle_event(event)
end
```

This script implements a simple FSM for controlling a robot. The FSM has four states: IDLE, MOVING, TURNING, and STOPPING. The transitions between the states are defined in the TRANSITIONS table. The initial state of the FSM is IDLE.

The handle_event function is responsible for handling events. When an event occurs, the function finds the transition that matches the current state and event. If a transition is found, the function updates the current state and executes the transition's action.

The action functions for the transitions are defined in the script. The action functions perform the desired actions for each transition.

The script runs the FSM in a loop. The loop waits for an event to occur and then calls the handle_event function to handle the event.