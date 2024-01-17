```csharp
// This code implements a simple text-based adventure game engine.

// The game world is represented as a graph of locations.
// Each location has a name, a description, and a list of exits.
// An exit is a connection between two locations.

// The player character is represented by a simple class with a name and a current location.

// The game engine is responsible for managing the game state,
// including the player's current location and the list of available exits.

// The game loop is a simple while loop that prompts the player for input,
// parses the input, and updates the game state accordingly.

using System;
using System.Collections.Generic;

namespace AdventureGame
{
    class Program
    {
        // The game world is represented as a graph of locations.
        private static Dictionary<string, Location> locations = new Dictionary<string, Location>();

        // The player character is represented by a simple class with a name and a current location.
        private static Player player = new Player("Player");

        // The game engine is responsible for managing the game state,
        // including the player's current location and the list of available exits.
        private static GameEngine gameEngine = new GameEngine();

        static void Main(string[] args)
        {
            // Create the game world.
            CreateWorld();

            // Start the game loop.
            while (true)
            {
                // Prompt the player for input.
                Console.Write("> ");
                string input = Console.ReadLine();

                // Parse the input.
                string[] words = input.Split(' ');
                string command = words[0];
                string argument = words.Length > 1 ? words[1] : null;

                // Update the game state accordingly.
                gameEngine.ProcessCommand(command, argument);
            }
        }

        private static void CreateWorld()
        {
            // Create the starting location.
            Location startingLocation = new Location("Starting Location");
            startingLocation.Description = "You are standing in a small, dark room.";
            startingLocation.Exits.Add("north", new Exit("North Exit", "North Room"));
            startingLocation.Exits.Add("south", new Exit("South Exit", "South Room"));

            // Create the north room.
            Location northRoom = new Location("North Room");
            northRoom.Description = "You are standing in a large, well-lit room.";
            northRoom.Exits.Add("south", new Exit("South Exit", "Starting Location"));

            // Create the south room.
            Location southRoom = new Location("South Room");
            southRoom.Description = "You are standing in a small, dark room.";
            southRoom.Exits.Add("north", new Exit("North Exit", "Starting Location"));

            // Add the locations to the game world.
            locations.Add(startingLocation.Name, startingLocation);
            locations.Add(northRoom.Name, northRoom);
            locations.Add(southRoom.Name, southRoom);

            // Set the player's starting location.
            player.Location = startingLocation;
        }
    }

    class Location
    {
        public string Name { get; set; }
        public string Description { get; set; }
        public Dictionary<string, Exit> Exits { get; set; }

        public Location(string name)
        {
            Name = name;
            Description = "";
            Exits = new Dictionary<string, Exit>();
        }
    }

    class Exit
    {
        public string Name { get; set; }
        public string Destination { get; set; }

        public Exit(string name, string destination)
        {
            Name = name;
            Destination = destination;
        }
    }

    class Player
    {
        public string Name { get; set; }
        public Location Location { get; set; }

        public Player(string name)
        {
            Name = name;
            Location = null;
        }
    }

    class GameEngine
    {
        public void ProcessCommand(string command, string argument)
        {
            switch (command)
            {
                case "look":
                    Look();
                    break;
                case "go":
                    Go(argument);
                    break;
                case "quit":
                    Quit();
                    break;
                default:
                    Console.WriteLine("I don't understand that command.");
                    break;
            }
        }

        private void Look()
        {
            Console.WriteLine(player.Location.Description);
        }

        private void Go(string direction)
        {
            if (player.Location.Exits.ContainsKey(direction))
            {
                player.Location = locations[player.Location.Exits[direction].Destination];
                Console.WriteLine(player.Location.Description);
            }
            else
            {
                Console.WriteLine("You can't go that way.");
            }
        }

        private void Quit()
        {
            Environment.Exit(0);
        }
    }
}
```

This code implements a simple text-based adventure game engine. The game world is represented as a graph of locations, where each location has a name, a description, and a list of exits. The player character is represented by a simple class with a name and a current location. The game engine is responsible for managing the game state, including the player's current location and the list of available exits. The game loop is a simple while loop that prompts the player for input, parses the input, and updates the game state accordingly.

The code is well-organized and easy to understand. It uses a modular design, with each class responsible for a specific part of the game. The code also uses a dictionary to store the game world, which makes it easy to add or remove locations.

This code is a good example of how to write a simple game engine in C#. It is also a good example of how to use a graph to represent a game world.