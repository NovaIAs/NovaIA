```kotlin
// Define a sealed class to represent the different types of events that can occur.
sealed class Event {
    data class MessageReceived(val message: String) : Event()
    data class UserJoined(val user: User) : Event()
    data class UserLeft(val user: User) : Event()
}

// Define a data class to represent a user.
data class User(val name: String)

// Define a function to handle the different types of events.
fun handleEvent(event: Event) {
    when (event) {
        is MessageReceived -> {
            println("Received message: ${event.message}")
        }
        is UserJoined -> {
            println("User joined: ${event.user.name}")
        }
        is UserLeft -> {
            println("User left: ${event.user.name}")
        }
    }
}

// Create a list of events.
val events = listOf(
    MessageReceived("Hello, world!"),
    UserJoined(User("Alice")),
    UserLeft(User("Bob")),
    MessageReceived("How are you?"),
    UserJoined(User("Charlie")),
    UserLeft(User("Alice"))
)

// Iterate over the list of events and handle each one.
events.forEach { handleEvent(it) }
```

**Explanation:**

This Kotlin code defines a sealed class called `Event` to represent different types of events that can occur. The `Event` class has three subclasses: `MessageReceived`, `UserJoined`, and `UserLeft`.

The `User` data class is used to represent a user.

The `handleEvent()` function is used to handle the different types of events. The `when` expression is used to match the event to the appropriate subclass and then handle the event accordingly.

The `events` list is a list of events that will be handled by the `handleEvent()` function.

The `forEach()` function is used to iterate over the list of events and call the `handleEvent()` function for each event.

This code demonstrates how to use sealed classes and the `when` expression to handle different types of events in Kotlin.

**Example Output:**

```
Received message: Hello, world!
User joined: Alice
User left: Bob
Received message: How are you?
User joined: Charlie
User left: Alice
```