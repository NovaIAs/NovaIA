```kotlin
fun main(args: Array<String>) {
    // Create a list of integers
    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // Create a map of strings to integers
    val map = mapOf("one" to 1, "two" to 2, "three" to 3, "four" to 4, "five" to 5)

    // Create a function that returns the square of a number
    val square: (Int) -> Int = { number -> number * number }

    // Print the list of numbers
    println("Numbers:")
    numbers.forEach { number -> println(number) }

    // Print the map of strings to integers
    println("Map:")
    map.forEach { key, value -> println("$key: $value") }

    // Print the squares of the numbers
    println("Squares:")
    numbers.map(square).forEach { square -> println(square) }

    // Find the sum of the numbers
    val sum = numbers.sum()
    println("Sum: $sum")

    // Find the average of the numbers
    val average = numbers.average()
    println("Average: $average")

    // Find the maximum value in the list
    val max = numbers.max()
    println("Maximum: $max")

    // Find the minimum value in the list
    val min = numbers.min()
    println("Minimum: $min")

    // Filter the list to only include even numbers
    val evenNumbers = numbers.filter { number -> number % 2 == 0 }
    println("Even numbers:")
    evenNumbers.forEach { number -> println(number) }

    // Sort the list in ascending order
    val sortedNumbers = numbers.sorted()
    println("Sorted numbers:")
    sortedNumbers.forEach { number -> println(number) }

    // Create a new list by combining two lists
    val combinedList = numbers + listOf(11, 12, 13, 14, 15)
    println("Combined list:")
    combinedList.forEach { number -> println(number) }

    // Create a new list by removing an element from a list
    val newList = numbers - 5
    println("New list:")
    newList.forEach { number -> println(number) }

    // Check if a list contains an element
    val contains = numbers.contains(7)
    println("Contains 7: $contains")

    // Check if a list is empty
    val isEmpty = numbers.isEmpty()
    println("Is empty: $isEmpty")

    // Get the size of a list
    val size = numbers.size
    println("Size: $size")

    // Get the index of an element in a list
    val indexOf = numbers.indexOf(5)
    println("Index of 5: $indexOf")

    // Get the last index of an element in a list
    val lastIndexOf = numbers.lastIndexOf(5)
    println("Last index of 5: $lastIndexOf")

    // Create a new list using a range
    val range = 1..10
    val rangeList = range.toList()
    println("Range list:")
    rangeList.forEach { number -> println(number) }

    // Create a new list using a step
    val step = 2
    val stepList = range.step(step).toList()
    println("Step list:")
    stepList.forEach { number -> println(number) }

    // Create a new list using a progression
    val progression = 1..10 step 2
    val progressionList = progression.toList()
    println("Progression list:")
    progressionList.forEach { number -> println(number) }

    // Create a new list using a generator function
    val generatorList = generateSequence { (it + 1) % 10 }.take(10).toList()
    println("Generator list:")
    generatorList.forEach { number -> println(number) }

    // Create a new list using a sequence
    val sequence = numbers.asSequence()
    val sequenceList = sequence.toList()
    println("Sequence list:")
    sequenceList.forEach { number -> println(number) }

    // Create a new list using a builder function
    val builderList = buildList {
        for (number in 1..10) {
            add(number)
        }
    }
    println("Builder list:")
    builderList.forEach { number -> println(number) }

    // Create a new list using a lambda expression
    val lambdaList = (1..10).map { it * 2 }.toList()
    println("Lambda list:")
    lambdaList.forEach { number -> println(number) }

    // Create a new list using a higher-order function
    val higherOrderList = numbers.map { square(it) }.toList()
    println("Higher-order list:")
    higherOrderList.forEach { number -> println(number) }

    // Create a new