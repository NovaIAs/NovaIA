```kotlin
// This code implements a simple text-based game engine in Kotlin.

// The main game class.
class Game {
    // The current state of the game.
    private var state: GameState = GameState.START

    // The player's current location.
    private var location: Location? = null

    // The player's inventory.
    private val inventory = mutableListOf<Item>()

    // The game's world.
    private val world = World()

    // The game's input handler.
    private val inputHandler = InputHandler()

    // The game's output handler.
    private val outputHandler = OutputHandler()

    // Start the game.
    fun start() {
        // Initialize the game state.
        state = GameState.PLAYING

        // Set the player's starting location.
        location = world.getStartLocation()

        // Display the game's introduction.
        outputHandler.printIntroduction()

        // Start the game loop.
        while (state == GameState.PLAYING) {
            // Get the player's input.
            val input = inputHandler.getInput()

            // Process the player's input.
            processInput(input)

            // Update the game state.
            updateState()

            // Display the game's output.
            outputHandler.printOutput()
        }

        // End the game.
        end()
    }

    // Process the player's input.
    private fun processInput(input: String) {
        // Split the input into words.
        val words = input.split(" ")

        // Get the first word of the input.
        val command = words[0]

        // Process the command.
        when (command) {
            "go" -> {
                // Get the second word of the input.
                val direction = words[1]

                // Move the player in the specified direction.
                movePlayer(direction)
            }
            "look" -> {
                // Look around the player's current location.
                lookAround()
            }
            "take" -> {
                // Get the second word of the input.
                val itemName = words[1]

                // Take the specified item from the player's current location.
                takeItem(itemName)
            }
            "use" -> {
                // Get the second word of the input.
                val itemName = words[1]

                // Use the specified item from the player's inventory.
                useItem(itemName)
            }
            "quit" -> {
                // End the game.
                end()
            }
            else -> {
                // Display an error message.
                outputHandler.printError("Invalid command.")
            }
        }
    }

    // Move the player in the specified direction.
    private fun movePlayer(direction: String) {
        // Get the player's current location.
        val currentLocation = location

        // Get the exit from the current location in the specified direction.
        val exit = currentLocation?.getExit(direction)

        // If the exit exists, move the player to the new location.
        if (exit != null) {
            location = exit.destination
        } else {
            // Display an error message.
            outputHandler.printError("You can't go that way.")
        }
    }

    // Look around the player's current location.
    private fun lookAround() {
        // Get the player's current location.
        val currentLocation = location

        // Display the description of the current location.
        outputHandler.printLocationDescription(currentLocation)

        // Display the exits from the current location.
        outputHandler.printExits(currentLocation)

        // Display the items in the current location.
        outputHandler.printItems(currentLocation)
    }

    // Take the specified item from the player's current location.
    private fun takeItem(itemName: String) {
        // Get the player's current location.
        val currentLocation = location

        // Get the specified item from the current location.
        val item = currentLocation?.getItem(itemName)

        // If the item exists, add it to the player's inventory and remove it from the current location.
        if (item != null) {
            inventory.add(item)
            currentLocation.removeItem(item)

            // Display a message indicating that the item was taken.
            outputHandler.printItemTaken(item)
        } else {
            // Display an error message.
            outputHandler.printError("You can't take that.")
        }
    }

    // Use the specified item from the player's inventory.
    private fun useItem(itemName: String) {
        // Get the specified item from the player's inventory.
        val item = inventory.find { it.name == itemName }

        // If the item exists, use it.
        if (item != null) {
            item.use()

            // Display a message indicating that the item was used.
            outputHandler.printItemUsed(item)
        } else {
            // Display an error message.
            outputHandler.printError("You don't have that.")
        }
    }

    // Update the game state.
    private fun updateState() {
        // Check if the player has won the game.
        if (location == world.getEndLocation()) {
            state = GameState.WON
        }

        // Check if the player has lost the game.
        if (location == world.getDeathLocation()) {
            state = GameState.LOST
        }
    }

    // End the game.
    private fun end() {
        // Display the game's ending.
        outputHandler.printEnding()

        // Exit the game.
        System.exit(0)
    }
}

// The game state.
enum class GameState {
    START,
    PLAYING,
    WON,
    LOST
}

// The game world.
class World {
    // The game's locations.
    private val locations = mutableMapOf<String, Location>()

    // The game's start location.
    private var startLocation: Location? = null

    // The game's end location.
    private var endLocation: Location? = null

    // The game's death location.
    private var deathLocation: Location? = null

    // Add a location to the world.
    fun addLocation(location: Location) {
        locations[location.name] = location
    }

    // Set the game's start location.
    fun setStartLocation(location: Location) {
        startLocation = location
    }

    // Set the game's end location.
    fun setEndLocation(location: Location) {
        endLocation = location
    }

    // Set the game's death location.
    fun setDeathLocation(location: Location) {
        deathLocation = location
    }

    // Get the game's start location.
    fun getStartLocation(): Location? {
        return startLocation
    }

    // Get the game's end location.
    fun getEndLocation(): Location? {
        return endLocation
    }

    // Get the game's death location.
    fun getDeathLocation(): Location? {
        return deathLocation
    }
}

// A location in the game world.
class Location {
    // The name of the location.
    val name: String

    // The description of the location.
    val description: String

    // The exits from the location.
    private val exits = mutableMapOf<String, Exit>()

    // The items in the location.
    private val items = mutableListOf<Item>()

    // Create a new location.
    constructor(name: String, description: String) {
        this.name = name
        this.description = description
    }

    // Add an exit to the location.
    fun addExit(exit: Exit) {
        exits[exit.direction] = exit
    }

    // Get the exit from the location in the specified direction.
    fun getExit(direction: String): Exit? {
        return exits[direction]
    }

    // Add an item to the location.
    fun addItem(item: Item) {
        items.add(item)
    }

    // Get the specified item from the location.
    fun getItem(itemName: String): Item? {
        return items.find { it.name == itemName }
    }

    // Remove the specified item from the location.
    fun removeItem(item: Item) {
        items.remove(item)
    }
}

// An exit from a location.
class Exit {
    // The direction of the exit.
    val direction: String

    // The destination of the exit.
    val destination: Location

    // Create a new exit.
    constructor(direction: String, destination: Location) {
        this.direction = direction
        this.destination = destination
    }
}