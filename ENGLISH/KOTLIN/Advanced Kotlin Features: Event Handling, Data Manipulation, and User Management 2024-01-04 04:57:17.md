```kotlin
// This code showcases advanced Kotlin features and is intended to be complex and varied.

// Define a sealed class representing different types of events.
sealed class Event {
    object LoginEvent : Event()
    class PurchaseEvent(val product: String, val price: Double) : Event()
    class RegistrationEvent(val username: String, val password: String) : Event()
}

// Define an interface for handling events.
interface EventHandler<T> {
    fun handle(event: T)
}

// Create a class that implements the EventHandler interface for each type of event.
class LoginEventHandler : EventHandler<Event.LoginEvent> {
    override fun handle(event: Event.LoginEvent) {
        println("User logged in.")
    }
}

class PurchaseEventHandler : EventHandler<Event.PurchaseEvent> {
    override fun handle(event: Event.PurchaseEvent) {
        println("User purchased ${event.product} for $${event.price}.")
    }
}

class RegistrationEventHandler : EventHandler<Event.RegistrationEvent> {
    override fun handle(event: Event.RegistrationEvent) {
        println("User registered with username ${event.username}.")
    }
}

// Create a function to handle events of different types.
fun handleEvent(event: Event) {
    when (event) {
        is Event.LoginEvent -> LoginEventHandler().handle(event)
        is Event.PurchaseEvent -> PurchaseEventHandler().handle(event)
        is Event.RegistrationEvent -> RegistrationEventHandler().handle(event)
    }
}

// Create a list of events and handle them using the handleEvent function.
val events = listOf(
    Event.LoginEvent,
    Event.PurchaseEvent("Book", 10.99),
    Event.RegistrationEvent("johndoe", "password123")
)

events.forEach { handleEvent(it) }

// Create a class that represents a user.
data class User(val username: String, val password: String)

// Create a function to validate a user's credentials.
fun validateCredentials(user: User): Boolean {
    return user.username == "johndoe" && user.password == "password123"
}

// Create a function that takes a list of users and returns a list of usernames.
fun extractUsernames(users: List<User>): List<String> {
    return users.map { it.username }
}

// Create a function that takes a list of users and returns a map of usernames to passwords.
fun createUserMap(users: List<User>): Map<String, String> {
    return users.associateBy({ it.username }, { it.password })
}

// Create a list of users.
val users = listOf(
    User("johndoe", "password123"),
    User("janedoe", "password456")
)

// Validate a user's credentials.
val isValid = validateCredentials(users[0])

// Extract usernames from the list of users.
val usernames = extractUsernames(users)

// Create a map of usernames to passwords from the list of users.
val userMap = createUserMap(users)

// Print the results.
println("Is user valid: $isValid")
println("Usernames: $usernames")
println("User map: $userMap")

```

Explanation:

1. We define a sealed class `Event` to represent different types of events.
2. We define an interface `EventHandler` for handling events.
3. We create classes for each type of event that implement the `EventHandler` interface.
4. We define a function `handleEvent` to handle events of different types.
5. We create a list of events and handle them using the `handleEvent` function.
6. We define a `User` data class to represent users.
7. We define functions to validate user credentials, extract usernames from a list of users, and create a map of usernames to passwords.
8. We create a list of users and perform various operations on it, such as validating credentials, extracting usernames, and creating a map of usernames to passwords.
9. We print the results of these operations.

This code demonstrates advanced Kotlin features such as sealed classes, interfaces, generics, lambda expressions, and data classes. It also showcases how to handle different types of events and perform operations on a list of objects.