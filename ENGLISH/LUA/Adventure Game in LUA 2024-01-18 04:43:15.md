```lua
-- This Lua script implements a simple text-based adventure game.
-- The player can explore a world, interact with objects and characters,
-- and solve puzzles to progress through the game.

-- Create the player character.
local player = {
    name = "Player",
    health = 100,
    inventory = {}
}

-- Define the world map.
local world = {
    [1] = {
        name = "Forest",
        description = "A dense forest with tall trees and a thick undergrowth.",
        exits = {
            north = 2,
            east = 3
        }
    },
    [2] = {
        name = "Clearing",
        description = "A small clearing in the forest with a few trees and a path leading north.",
        exits = {
            north = 3,
            south = 1
        }
    },
    [3] = {
        name = "Cave Entrance",
        description = "The entrance to a dark cave.",
        exits = {
            east = 4,
            south = 2
        }
    },
    [4] = {
        name = "Cave",
        description = "A dark and damp cave with a few stalactites and stalagmites.",
        exits = {
            west = 3
        }
    }
}

-- Define the objects in the world.
local objects = {
    [1] = {
        name = "Sword",
        description = "A sharp sword that can be used to attack enemies.",
        location = 3
    },
    [2] = {
        name = "Health Potion",
        description = "A potion that can be used to restore health.",
        location = 4
    },
    [3] = {
        name = "Key",
        description = "A key that can be used to open a door.",
        location = 2
    },
    [4] = {
        name = "Chest",
        description = "A locked chest that contains a treasure.",
        location = 4,
        locked = true
    }
}

-- Define the characters in the world.
local characters = {
    [1] = {
        name = "Guard",
        description = "A guard who stands at the cave entrance.",
        dialogue = {
            ["default"] = "Halt! Who goes there?",
            ["sword"] = "You shall not pass without my permission!",
            ["key"] = "Ah, you have the key. You may pass.",
        }
    }
}

-- Define the game state.
local game_state = {
    current_location = 1,
    inventory = {}
}

-- Function to print the game introduction.
function print_intro()
    print("Welcome to the Adventure Game!")
    print("You are a brave adventurer who is exploring a dangerous world.")
    print("Your goal is to find the treasure chest and return it to its rightful owner.")
    print("Be careful, there are many dangers lurking in the shadows.")
end

-- Function to print the current location description.
function print_location_description()
    local location = world[game_state.current_location]
    print(location.description)
end

-- Function to print the available exits.
function print_exits()
    local location = world[game_state.current_location]
    print("Available exits:")
    for direction, destination in pairs(location.exits) do
        print(string.format("%s: %s", direction, world[destination].name))
    end
end

-- Function to get the player's input.
function get_input()
    io.write("> ")
    return io.read()
end

-- Function to process the player's input.
function process_input(input)
    local command, argument = input:match("^(%w+)(?: (.*))?$")
    if command == "go" then
        handle_go_command(argument)
    elseif command == "take" then
        handle_take_command(argument)
    elseif command == "use" then
        handle_use_command(argument)
    elseif command == "talk" then
        handle_talk_command(argument)
    elseif command == "quit" then
        handle_quit_command()
    else
        print("Invalid command.")
    end
end

-- Function to handle the "go" command.
function handle_go_command(direction)
    local location = world[game_state.current_location]
    if location.exits[direction] then
        game_state.current_location = location.exits[direction]
        print_location_description()
        print_exits()
    else
        print("You cannot go that way.")
    end
end

-- Function to handle the "take" command.
function handle_take_command(object_name)
    local object = find_object(object_name)
    if object and object.location == game_state.current_location then
        table.insert(game_state.inventory, object)
        object.location = nil
        print(string.format("You took the %s.", object.name))
    else