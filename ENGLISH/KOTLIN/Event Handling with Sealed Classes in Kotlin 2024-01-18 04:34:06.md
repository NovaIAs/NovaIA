```kotlin
// Define a data class to represent a person with name and age.
data class Person(var name: String, var age: Int)

// Define a sealed class to represent different types of events that can occur.
sealed class Event {
    object Birthday : Event() // Event representing a birthday.
    data class Party(val name: String) : Event() // Event representing a party with a given name.
    object Wedding : Event() // Event representing a wedding.
}

// Define a function to handle different types of events.
fun handleEvent(event: Event) {
    when (event) {
        is Event.Birthday -> println("Happy birthday to ${event.name}!") // Handle birthday event.
        is Event.Party -> println("Party time with ${event.name}!") // Handle party event.
        is Event.Wedding -> println("Congratulations on your wedding!") // Handle wedding event.
    }
}

// Create a list of events.
val events = listOf(
    Event.Birthday("Alice", 25),
    Event.Party("Bob"),
    Event.Wedding("John")
)

// Iterate over the list of events and handle each one.
events.forEach { handleEvent(it) }
```

### Explanation:

1. **Data Class `Person`:**
   - A `data class` is used to represent a person with properties `name` and `age`.

2. **Sealed Class `Event`:**
   - A `sealed class` is used to define different event types that can occur.
   - Each event type is represented by an object or a data class.

3. **Function `handleEvent`:**
   - This function takes an `Event` object as its argument.
   - It uses the `when` expression to handle different event types based on their specific implementations.

4. **Event Handling:**
   - For each event type, a specific action is performed.

5. **List of Events `events`:**
   - A list of different events is created.

6. **Event Handling Loop:**
   - The `forEach` function is used to iterate over the list of events and handle each event by calling the `handleEvent` function.

This code demonstrates advanced features of Kotlin, including data classes, sealed classes, pattern matching with `when`, and event handling using a loop.