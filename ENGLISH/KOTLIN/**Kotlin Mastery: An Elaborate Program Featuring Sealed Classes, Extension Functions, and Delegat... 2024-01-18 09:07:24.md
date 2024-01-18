**Objective:** Generate a sophisticated Kotlin code that stands out with its unique features and complexity, ensuring minimal duplication in the code.

**Problem Statement:** Develop a Kotlin program that:

1. Employs a custom `MessageBuilder` class to construct a complex message.
2. Demonstrates the use of sealed classes, `when` expressions, and lambda functions.
3. Utilizes extension functions to enhance the functionality of the `String` class.
4. Applies the concept of delegation to simplify code and improve readability.
5. Includes unit tests to validate the functionality of the program.

**Kotlin Code:**

```kotlin
// Custom Message Builder Class
class MessageBuilder {

    // Builder Design Pattern
    private var message: String = ""

    fun addGreeting(recipient: String): MessageBuilder {
        message += "Hello, $recipient!"
        return this
    }

    fun addBody(body: String): MessageBuilder {
        message += "\n$body"
        return this
    }

    fun addClosing(sender: String): MessageBuilder {
        message += "\n\nSincerely,\n$sender"
        return this
    }

    fun build(): String {
        return message
    }
}

// Sealed Classes for Message Types
sealed class MessageType {
    object Welcome : MessageType()
    object Confirmation : MessageType()
    object Error : MessageType()
}

// Function to Generate a Message
fun generateMessage(type: MessageType, recipient: String, body: String, sender: String): String {
    when (type) {
        MessageType.Welcome -> return "Welcome to the club, $recipient!"
        MessageType.Confirmation -> return "Your order has been confirmed, $recipient. Order details: $body"
        MessageType.Error -> return "An error occurred, $recipient. Error message: $body"
    }
}

// Extension Function to Capitalize the First Letter of a String
fun String.capitalizeFirstChar() = this[0].toUpperCase() + substring(1)

// Delegation for Message Formatting
class FormattedMessage(val message: String) {
    val formattedMessage: String by lazy {
        message.capitalizeFirstChar()
    }
}

// Unit Tests
class MessageBuilderTest {

    @Test
    fun testMessageBuilder() {
        val message = MessageBuilder()
            .addGreeting("John")
            .addBody("How are you doing today?")
            .addClosing("Jane")
            .build()

        assertEquals("Hello, John!\nHow are you doing today?\n\nSincerely,\nJane", message)
    }

    @Test
    fun testGenerateMessage() {
        val welcomeMessage = generateMessage(MessageType.Welcome, "John", "", "")
        assertEquals("Welcome to the club, John!", welcomeMessage)

        val confirmationMessage = generateMessage(
            MessageType.Confirmation,
            "Jane",
            "Your order has been shipped",
            "John"
        )
        assertEquals("Your order has been confirmed, Jane. Order details: Your order has been shipped", confirmationMessage)

        val errorMessage = generateMessage(MessageType.Error, "Jack", "An error occurred", "System")
        assertEquals("An error occurred, Jack. Error message: An error occurred", errorMessage)
    }

    @Test
    fun testStringExtensionFunction() {
        val capitalizedString = "hello".capitalizeFirstChar()
        assertEquals("Hello", capitalizedString)
    }

    @Test
    fun testFormattedMessage() {
        val formattedMessage = FormattedMessage("hello world").formattedMessage
        assertEquals("Hello World", formattedMessage)
    }
}
```

**Explanation:**

1. **Message Builder:** This class utilizes the Builder Design Pattern to construct complex messages in a step-by-step manner.

2. **Sealed Classes and `when` Expressions:** Sealed classes are employed to define message types, and a `when` expression is used to generate the message based on the type.

3. **Extension Function:** An extension function is defined to capitalize the first letter of a string, demonstrating how to extend the functionality of existing classes.

4. **Delegation:** The `FormattedMessage` class delegates the formatting of the message to a `lazy` property, promoting code simplicity and readability.

5. **Unit Tests:** Unit tests are included to validate the functionality of the program.

This code showcases the versatility of Kotlin by incorporating advanced concepts like sealed classes, delegation, extension functions, and the Builder Design Pattern. It stands out with its uniqueness and demonstrates the power of Kotlin in tackling complex programming challenges.