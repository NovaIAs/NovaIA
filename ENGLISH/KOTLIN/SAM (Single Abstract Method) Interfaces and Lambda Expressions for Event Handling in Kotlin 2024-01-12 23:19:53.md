```kotlin
// Kotlin program to demonstrate the use of SAM (Single Abstract Method)
// interfaces and lambda expressions to create event handlers

// Interface for a lambda expression that takes an Int and returns nothing
interface OnClickListener {
    fun onClick(position: Int)
}

// Class representing a button with an onClick event handler
class Button(private val onClick: OnClickListener) {

    // Function to simulate the button being clicked
    fun click() {
        onClick.onClick(10)  // Call the onClick method of the OnClickListener instance
    }
}

// Main class
fun main(args: Array<String>) {

    // Create an instance of the Button class, passing in a lambda expression as the onClick event handler
    val button = Button(object : OnClickListener {
        override fun onClick(position: Int) {
            // Code to be executed when the button is clicked
            println("Button clicked at position $position")
        }
    })

    // Simulate the button being clicked
    button.click()
}
```

Explanation:

1. Interface `OnClickListener`: This interface defines a single abstract method `onClick`, which takes an `Int` as a parameter and returns nothing. This interface represents an event handler for a button click.

2. Class `Button`: This class represents a button with an `onClick` event handler. It has a private property `onClick` of type `OnClickListener`, which stores the event handler for the button.

3. Function `click()`: This function simulates the button being clicked. It calls the `onClick` method of the `OnClickListener` instance, passing in a value of 10 as the position of the button click.

4. Main Function:

   - It creates an instance of the `Button` class, passing in a lambda expression as the `onClick` event handler.
   - The lambda expression defines the code to be executed when the button is clicked. In this case, it prints a message to the console.
   - The button is then simulated to be clicked by calling the `click()` function.

This code demonstrates the use of SAM interfaces and lambda expressions in Kotlin to create event handlers for UI components. It shows how to define an interface with a single abstract method, create an instance of a class that implements the interface, and use a lambda expression to define the implementation of the event handler method.