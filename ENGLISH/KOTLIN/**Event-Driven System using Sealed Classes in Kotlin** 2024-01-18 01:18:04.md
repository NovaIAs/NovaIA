```kotlin
// Define a sealed class to represent different types of events that can occur in the system
sealed class Event {
    // Define a subclass for each type of event
    class UserLoggedIn(val user: User) : Event()
    class OrderCreated(val order: Order) : Event()
    class OrderShipped(val order: Order) : Event()
    class OrderDelivered(val order: Order) : Event()
    // Add more event subclasses as needed...
}

// Define a class to represent users in the system
data class User(val id: Int, val name: String, val email: String)

// Define a class to represent orders in the system
data class Order(val id: Int, val items: List<Item>, val total: Double)

// Define a class to represent items in the system
data class Item(val id: Int, val name: String, val price: Double)

// Define a function to handle events in the system
fun handleEvent(event: Event) {
    when (event) {
        is UserLoggedIn -> {
            // Handle the user login event
            println("User ${event.user.name} logged in")
        }
        is OrderCreated -> {
            // Handle the order creation event
            println("Order ${event.order.id} created")
        }
        is OrderShipped -> {
            // Handle the order shipped event
            println("Order ${event.order.id} shipped")
        }
        is OrderDelivered -> {
            // Handle the order delivered event
            println("Order ${event.order.id} delivered")
        }
        // Add more event handling cases as needed...
    }
}

// Sample usage of the event handling system
val event1 = UserLoggedIn(User(1, "John Doe", "johndoe@example.com"))
val event2 = OrderCreated(Order(1, listOf(Item(1, "Product A", 10.0), Item(2, "Product B", 20.0)), 30.0))
val event3 = OrderShipped(Order(1, listOf(Item(1, "Product A", 10.0), Item(2, "Product B", 20.0)), 30.0))
val event4 = OrderDelivered(Order(1, listOf(Item(1, "Product A", 10.0), Item(2, "Product B", 20.0)), 30.0))

handleEvent(event1)
handleEvent(event2)
handleEvent(event3)
handleEvent(event4)
```

Explanation of the code:

1. We define a sealed class called `Event` to represent different types of events that can occur in the system. This class has several subclasses, each representing a specific type of event. For example, we have `UserLoggedIn`, `OrderCreated`, `OrderShipped`, and `OrderDelivered` event subclasses.

2. We define data classes for `User`, `Order`, and `Item` to represent users, orders, and items in the system, respectively. These classes include various properties and constructors to store and manipulate data related to users, orders, and items.

3. We define a function called `handleEvent` to handle events in the system. This function takes an event as an argument and uses a `when` expression to handle different types of events based on their specific subclasses. For example, if the event is a `UserLoggedIn` event, it prints a message indicating that the user logged in.

4. We define several sample events and pass them to the `handleEvent` function to demonstrate how the event handling system works.