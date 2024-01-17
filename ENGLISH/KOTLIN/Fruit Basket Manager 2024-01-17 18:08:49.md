```kotlin
// Define a sealed class to represent different types of fruits
sealed class Fruit {
    object Apple : Fruit()
    object Orange : Fruit()
    object Banana : Fruit()
}

// Define a data class to represent a fruit basket
data class FruitBasket(val fruits: List<Fruit>) {
    // Function to count the number of fruits in the basket
    fun countFruits() = fruits.size

    // Function to get the total weight of the fruits in the basket
    fun getTotalWeight(): Double {
        return fruits.sumOf {
            when (it) {
                is Apple -> 0.2
                is Orange -> 0.3
                is Banana -> 0.1
            }
        }
    }

    // Function to get the most common fruit in the basket
    fun getMostCommonFruit(): Fruit? {
        val fruitCounts = fruits.groupBy { it }.mapValues { it.value.size }
        val maxCount = fruitCounts.values.maxOrNull() ?: return null
        return fruitCounts.filterValues { it == maxCount }.keys.firstOrNull()
    }
}

// Function to create a fruit basket with a specified list of fruits
fun createFruitBasket(fruits: List<Fruit>): FruitBasket {
    return FruitBasket(fruits)
}

// Function to print the details of a fruit basket
fun printFruitBasketDetails(basket: FruitBasket) {
    println("Number of fruits in the basket: ${basket.countFruits()}")
    println("Total weight of the fruits in the basket: ${basket.getTotalWeight()} kg")
    println("Most common fruit in the basket: ${basket.getMostCommonFruit()}")
}

// Create a fruit basket with a variety of fruits
val basket = createFruitBasket(listOf(Fruit.Apple, Fruit.Orange, Fruit.Banana, Fruit.Apple, Fruit.Orange))

// Print the details of the fruit basket
printFruitBasketDetails(basket)
```

Explanation:

1. We define a sealed class `Fruit` to represent different types of fruits. We can add new fruit types by adding new objects to this class.

2. We create a `FruitBasket` data class to represent a basket of fruits. The `fruits` property is a list of `Fruit` objects.

3. We define several functions in the `FruitBasket` class to perform various operations on the fruits in the basket:
   - `countFruits()`: Counts the number of fruits in the basket.
   - `getTotalWeight()`: Calculates the total weight of the fruits in the basket based on their individual weights.
   - `getMostCommonFruit()`: Finds the most common fruit in the basket.

4. We create a function `createFruitBasket()` that takes a list of `Fruit` objects and returns a `FruitBasket` instance.

5. We define a function `printFruitBasketDetails()` to print the details of a fruit basket, including the number of fruits, total weight, and most common fruit.

6. We create a fruit basket with a variety of fruits using the `createFruitBasket()` function and assign it to the variable `basket`.

7. Finally, we call the `printFruitBasketDetails()` function to display the details of the fruit basket.