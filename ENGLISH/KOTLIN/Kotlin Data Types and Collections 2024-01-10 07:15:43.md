```kotlin
// Kotlin program to demonstrate the use of Kotlin's
// built-in data types

// Main function
fun main(args: Array<String>) {
    // Boolean data type
    var isAwesome: Boolean = true

    // Byte data type
    var age: Byte = 25

    // Short data type
    var height: Short = 170

    // Int data type
    var population: Int = 1000000

    // Long data type
    var nationalDebt: Long = 200000000000000

    // Float data type
    var averageTemperature: Float = 22.5f

    // Double data type
    var gdp: Double = 1000000000000.0

    // Char data type
    var initial: Char = 'K'

    // String data type
    var name: String = "Kotlin"

    // Array data type
    var numbers: Array<Int> = arrayOf(1, 2, 3, 4, 5)

    // List data type
    var fruits: List<String> = listOf("Apple", "Orange", "Banana", "Grape")

    // Set data type
    var colors: Set<String> = setOf("Red", "Green", "Blue", "Yellow")

    // Map data type
    var capitals: Map<String, String> = mapOf("India" to "New Delhi", "USA" to "Washington D.C.", "China" to "Beijing")

    // Printing the values of the variables
    println("Boolean: $isAwesome")
    println("Byte: $age")
    println("Short: $height")
    println("Int: $population")
    println("Long: $nationalDebt")
    println("Float: $averageTemperature")
    println("Double: $gdp")
    println("Char: $initial")
    println("String: $name")
    println("Array: ${numbers.contentToString()}")
    println("List: $fruits")
    println("Set: $colors")
    println("Map: $capitals")
}
```

**Explanation:**

1. **Data Types:**
   - We declare variables of different data types available in Kotlin:
     - `Boolean`: `isAwesome` to store a boolean value (true or false).
     - `Byte`: `age` to store a byte value (8-bit signed integer).
     - `Short`: `height` to store a short value (16-bit signed integer).
     - `Int`: `population` to store an integer value (32-bit signed integer).
     - `Long`: `nationalDebt` to store a long value (64-bit signed integer).
     - `Float`: `averageTemperature` to store a floating-point value (32-bit precision).
     - `Double`: `gdp` to store a double-precision floating-point value (64-bit precision).
     - `Char`: `initial` to store a character value.
     - `String`: `name` to store a string value.

2. **Arrays:**
   - We declare an array `numbers` of `Int` type and initialize it with values.

3. **Lists:**
   - We declare a list `fruits` of `String` type and initialize it with values.

4. **Sets:**
   - We declare a set `colors` of `String` type and initialize it with values.

5. **Maps:**
   - We declare a map `capitals` of `String` keys and `String` values and initialize it with key-value pairs.

6. **Printing Values:**
   - We use `println()` to print the values of the variables to the console.

This code demonstrates the use of various data types, arrays, lists, sets, and maps in Kotlin. It shows how to declare and initialize variables of different types and how to print their values.