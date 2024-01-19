```kotlin
package com.example.complexkotlincode

import android.os.Bundle
import android.util.Log
import androidx.appcompat.app.AppCompatActivity
import kotlinx.coroutines.*
import java.util.*
import kotlin.collections.ArrayList

class MainActivity : AppCompatActivity() {

    // Coroutines
    private val coroutineScope = CoroutineScope(Job() + Dispatchers.IO)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        // Lambdas
        val addNumbers = { a: Int, b: Int -> a + b }
        val result = addNumbers(1, 2)
        Log.d("MainActivity", "Result: $result")

        // Extensions
        val list = ArrayList<Int>()
        list.add(1)
        list.add(2)
        list.add(3)
        val sum = list.sum()
        Log.d("MainActivity", "Sum: $sum")

        // Higher-Order Functions
        val numbers = listOf(1, 2, 3, 4, 5)
        val evenNumbers = numbers.filter { it % 2 == 0 }
        Log.d("MainActivity", "Even numbers: $evenNumbers")

        // Coroutines
        coroutineScope.launch {
            val result = async { longRunningTask() }
            val finalResult = result.await()
            Log.d("MainActivity", "Final result: $finalResult")
        }

        // Data Classes
        val user = User("John", "Doe", 30)
        Log.d("MainActivity", "User: $user")

        // Sealed Classes
        sealed class Shape {
            object Circle : Shape()
            object Square : Shape()
            object Rectangle : Shape()
        }
        val shape: Shape = Shape.Circle
        when (shape) {
            is Shape.Circle -> Log.d("MainActivity", "Shape is a circle")
            is Shape.Square -> Log.d("MainActivity", "Shape is a square")
            is Shape.Rectangle -> Log.d("MainActivity", "Shape is a rectangle")
        }

        // Generics
        val list2 = ArrayList<String>()
        list2.add("Hello")
        list2.add("World")
        val string = list2[0]
        Log.d("MainActivity", "String: $string")

        // Inline Functions
        inline fun greet(name: String) {
            Log.d("MainActivity", "Hello, $name!")
        }
        greet("Kotlin")

        // Object Expressions
        val person = object {
            val name = "Jane"
            val age = 25
            fun greet() {
                Log.d("MainActivity", "Hello, my name is $name and I am $age years old")
            }
        }
        person.greet()
    }

    private suspend fun longRunningTask(): Int {
        delay(5000L)
        return Random().nextInt(100)
    }

    data class User(val firstName: String, val lastName: String, val age: Int)
}
```
This code demonstrates various advanced Kotlin features:

- **Coroutines**: Uses coroutines to perform a long-running task asynchronously.

- **Lambdas**: Defines a lambda expression to add two numbers.

- **Extensions**: Extends the `ArrayList` class with a `sum()` function to calculate the sum of its elements.

- **Higher-Order Functions**: Filters a list of numbers to find even numbers using the `filter()` function.

- **Data Classes**: Defines a data class `User` with properties `firstName`, `lastName`, and `age`.

- **Sealed Classes**: Defines a sealed class `Shape` with three subclasses `Circle`, `Square`, and `Rectangle`.

- **Generics**: Uses generics to define an `ArrayList` that can hold elements of type `String`.

- **Inline Functions**: Defines an inline function `greet()` to print a greeting message.

- **Object Expressions**: Creates an anonymous object to represent a person with properties and a method.