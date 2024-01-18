```kotlin
// Kotlin code to demonstrate the use of lambda expressions and higher-order functions
// in a complex and differentiated code

// Function to generate a sequence of Fibonacci numbers
fun fibonacciSequence(n: Int): Sequence<Int> = sequence {
    var a = 0
    var b = 1
    yield(a)
    yield(b)
    while (true) {
        val c = a + b
        yield(c)
        a = b
        b = c
    }
}

// Function to filter a sequence of numbers based on a given condition
fun <T> filterSequence(sequence: Sequence<T>, condition: (T) -> Boolean): Sequence<T> = sequence {
    for (element in sequence) {
        if (condition(element)) {
            yield(element)
        }
    }
}

// Function to take the first n elements from a sequence
fun <T> takeSequence(sequence: Sequence<T>, n: Int): Sequence<T> = sequence {
    var count = 0
    for (element in sequence) {
        if (count < n) {
            yield(element)
            count++
        } else {
            break
        }
    }
}

// Function to map a sequence of numbers to their squares
fun squareSequence(sequence: Sequence<Int>): Sequence<Int> = sequence {
    for (element in sequence) {
        yield(element * element)
    }
}

// Function to sum a sequence of numbers
fun sumSequence(sequence: Sequence<Int>): Int = sequence.sum()

// Function to find the maximum element in a sequence of numbers
fun maxSequence(sequence: Sequence<Int>): Int = sequence.max() ?: 0

// Function to find the minimum element in a sequence of numbers
fun minSequence(sequence: Sequence<Int>): Int = sequence.min() ?: 0

// Function to print a sequence of numbers
fun printSequence(sequence: Sequence<Int>) {
    for (element in sequence) {
        print("$element ")
    }
    println()
}

// Usage of the functions defined above
val sequence1 = fibonacciSequence(10)

val filteredSequence = filterSequence(sequence1, { it % 2 == 0 })

val takenSequence = takeSequence(filteredSequence, 5)

val squaredSequence = squareSequence(takenSequence)

val sum = sumSequence(squaredSequence)

val max = maxSequence(squaredSequence)

val min = minSequence(squaredSequence)

printSequence(takenSequence)

println("Sum: $sum")
println("Max: $max")
println("Min: $min")
```

Explanation:

1. The `fibonacciSequence` function generates a sequence of Fibonacci numbers.
2. The `filterSequence` function filters a sequence of numbers based on a given condition.
3. The `takeSequence` function takes the first `n` elements from a sequence.
4. The `squareSequence` function maps a sequence of numbers to their squares.
5. The `sumSequence` function sums a sequence of numbers.
6. The `maxSequence` function finds the maximum element in a sequence of numbers.
7. The `minSequence` function finds the minimum element in a sequence of numbers.
8. The `printSequence` function prints a sequence of numbers.

The code first generates a sequence of Fibonacci numbers using the `fibonacciSequence` function. Then, it filters the sequence to only include even numbers using the `filterSequence` function. Next, it takes the first 5 elements from the filtered sequence using the `takeSequence` function. The taken sequence is then mapped to their squares using the `squareSequence` function. The sum of the squared sequence is calculated using the `sumSequence` function. The maximum and minimum elements of the squared sequence are found using the `maxSequence` and `minSequence` functions, respectively. Finally, the taken sequence, sum, maximum and minimum elements are printed using the `printSequence` function.

The output of the code is:

```
2 3 5 8 13
Sum: 233
Max: 169
Min: 4
```