```lua
-- This Lua code implements a simple text-based adventure game engine.

-- Define the player's initial state.
local player = {
  name = "Player",
  health = 100,
  inventory = {},
  location = "Forest"
}

-- Define the game world.
local world = {
  Forest = {
    description = "You are in a lush forest.",
    exits = {
      north = "Meadow",
      south = "Cave"
    }
  },
  Meadow = {
    description = "You are in a grassy meadow.",
    exits = {
      south = "Forest",
      east = "Mountain"
    }
  },
  Cave = {
    description = "You are in a dark cave.",
    exits = {
      north = "Forest"
    }
  },
  Mountain = {
    description = "You are on a rocky mountain.",
    exits = {
      west = "Meadow"
    }
  }
}

-- Define the game's commands.
local commands = {
  go = function(args)
    if world[player.location].exits[args[1]] then
      player.location = world[player.location].exits[args[1]]
      print(world[player.location].description)
    else
      print("You can't go that way.")
    end
  end,
  look = function(args)
    print(world[player.location].description)
  end,
  inventory = function(args)
    if player.inventory then
      print("You are carrying:")
      for item in pairs(player.inventory) do
        print(" - " .. item)
      end
    else
      print("You are not carrying anything.")
    end
  end,
  help = function(args)
    print("Available commands:")
    for command in pairs(commands) do
      print(" - " .. command)
    end
  end,
  quit = function(args)
    os.exit(0)
  end
}

-- Start the game loop.
while true do
  -- Prompt the player for a command.
  print("> ")
  local input = io.read()

  -- Parse the player's command.
  local command, args = input:match("^(%w+)(?: (.*))?$")

  -- Execute the player's command.
  if commands[command] then
    commands[command](args)
  else
    print("Unknown command.")
  end
end
```

This code is a simple text-based adventure game engine. It defines the player's initial state, the game world, the game's commands, and the game loop.

The player's initial state is defined as a Lua table with the following keys:

* `name`: The player's name.
* `health`: The player's health.
* `inventory`: The player's inventory.
* `location`: The player's current location.

The game world is defined as a Lua table with the following keys:

* Each key is a location in the game world.
* Each value is a Lua table with the following keys:
    * `description`: A description of the location.
    * `exits`: A table of exits from the location.

The game's commands are defined as a Lua table with the following keys:

* Each key is a command that the player can enter.
* Each value is a Lua function that executes the command.

The game loop is a while loop that prompts the player for a command, parses the command, and executes the command.

This code is a good example of how to use Lua to create a simple game. It is also a good example of how to use Lua tables to store data.