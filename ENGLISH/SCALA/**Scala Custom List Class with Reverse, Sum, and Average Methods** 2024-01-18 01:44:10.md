```scala
// Define a custom list type, which extends the standard List class.
class MyList[T] extends List[T] {
  // Define a custom method to reverse the list.
  def reverse: MyList[T] = {
    // Create a new empty list.
    val newList = new MyList[T]()

    // Iterate over the current list in reverse order.
    for (i <- (this.length - 1) to 0 by -1) {
      // Add the element at the current index to the new list.
      newList = newList :+ this(i)
    }

    // Return the new reversed list.
    newList
  }

  // Define a custom method to calculate the sum of all the elements in the list.
  def sum: Double = {
    // Initialize the sum to 0.
    var sum = 0.0

    // Iterate over the list and add each element to the sum.
    for (element <- this) {
      // Check if the element is a Double.
      if (element.isInstanceOf[Double]) {
        // If it is a Double, add it to the sum.
        sum += element.asInstanceOf[Double]
      }
    }

    // Return the sum.
    sum
  }

  // Define a custom method to calculate the average of all the elements in the list.
  def average: Double = {
    // Check if the list is empty.
    if (this.isEmpty) {
      // If the list is empty, return 0.
      0.0
    } else {
      // Calculate the sum of all the elements in the list.
      val sum = this.sum

      // Calculate the average by dividing the sum by the number of elements in the list.
      sum / this.length
    }
  }
}

// Create a new MyList object.
val myList = new MyList[Int]()

// Add some elements to the list.
myList += 1
myList += 2
myList += 3
myList += 4
myList += 5

// Print the original list.
println("Original list:")
println(myList)

// Reverse the list using the reverse method.
val reversedList = myList.reverse

// Print the reversed list.
println("Reversed list:")
println(reversedList)

// Calculate the sum of all the elements in the list using the sum method.
val sum = myList.sum

// Print the sum.
println("Sum of all the elements in the list:")
println(sum)

// Calculate the average of all the elements in the list using the average method.
val average = myList.average

// Print the average.
println("Average of all the elements in the list:")
println(average)
```

**Explanation:**

The code defines a custom list type called `MyList`, which extends the standard `List` class. This allows us to add custom methods to the `List` class.

The `MyList` class defines three custom methods:

* `reverse`: This method reverses the order of the elements in the list.
* `sum`: This method calculates the sum of all the elements in the list.
* `average`: This method calculates the average of all the elements in the list.

The code then creates a new `MyList` object and adds some elements to it. It then uses the `reverse`, `sum`, and `average` methods to manipulate the list and print out the results.

This code demonstrates some of the powerful features of Scala, including the ability to define custom classes and methods, and to use those methods to manipulate and analyze data structures.