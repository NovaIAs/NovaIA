```scala
// Define a sealed trait to represent different types of events
sealed trait Event

// Define concrete case classes for different types of events
case class UserLoggedIn(username: String, timestamp: Long) extends Event
case class UserLoggedOut(username: String, timestamp: Long) extends Event
case class ItemPurchased(item: String, price: Double, quantity: Int, timestamp: Long) extends Event
case class ItemAddedToCart(item: String, price: Double, quantity: Int, timestamp: Long) extends Event

// Define a function to process events
def processEvent(event: Event): Unit = {
  event match {
    case UserLoggedIn(username, timestamp) =>
      println(s"$username logged in at $timestamp")

    case UserLoggedOut(username, timestamp) =>
      println(s"$username logged out at $timestamp")

    case ItemPurchased(item, price, quantity, timestamp) =>
      println(s"$item purchased for $price x $quantity at $timestamp")

    case ItemAddedToCart(item, price, quantity, timestamp) =>
      println(s"$item added to cart for $price x $quantity at $timestamp")
  }
}

// Create a list of events
val events = List(
  UserLoggedIn("alice", 1658012800000L),
  ItemAddedToCart("apple", 0.99, 2, 1658012860000L),
  ItemPurchased("banana", 1.99, 3, 1658012920000L),
  UserLoggedOut("alice", 1658013040000L),
  ItemAddedToCart("orange", 0.49, 5, 1658013100000L),
  ItemPurchased("apple", 0.99, 1, 1658013160000L)
)

// Process each event in the list
events.foreach(processEvent)
```
Explanation:

1. We define a sealed trait Event to represent different types of events. This trait is sealed to ensure that no new subclasses can be defined outside of this file.

2. We define concrete case classes for different types of events:

   - UserLoggedIn: Represents an event when a user logs in.
   - UserLoggedOut: Represents an event when a user logs out.
   - ItemPurchased: Represents an event when an item is purchased.
   - ItemAddedToCart: Represents an event when an item is added to the cart.

3. We define a function processEvent that takes an Event as an argument and processes it. The function uses pattern matching to handle different types of events.

4. We create a list of events containing instances of the concrete case classes.

5. We call the foreach method on the events list to process each event using the processEvent function.

When you run this code, it will process each event in the list and print a message to the console.