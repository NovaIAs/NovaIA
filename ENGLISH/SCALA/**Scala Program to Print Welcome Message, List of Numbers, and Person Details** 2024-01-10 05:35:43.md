```scala
// This is a multi-line comment in Scala.

// Define an object named "Main".
object Main {

  // Define a main method, which is the entry point of the program.
  def main(args: Array[String]): Unit = {

    // Print a welcome message to the console.
    println("Welcome to Scala!")

    // Define a variable named "name" and assign it the value "John Doe".
    val name: String = "John Doe"

    // Print the value of the "name" variable to the console.
    println(s"Hello, $name!")

    // Define a list of numbers.
    val numbers: List[Int] = List(1, 2, 3, 4, 5)

    // Print the list of numbers to the console.
    println(s"Numbers: $numbers")

    // Iterate over the list of numbers and print each number to the console.
    numbers.foreach(println)

    // Define a function named "sum" that takes two numbers as arguments and returns their sum.
    def sum(a: Int, b: Int): Int = {
      a + b
    }

    // Call the "sum" function with the numbers 1 and 2 and print the result to the console.
    println(s"Sum of 1 and 2: ${sum(1, 2)}")

    // Define a class named "Person".
    class Person(val name: String, val age: Int) {

      // Define a method named "greet" that prints a greeting message to the console.
      def greet(): Unit = {
        println(s"Hello, my name is $name and I am $age years old.")
      }
    }

    // Create an instance of the "Person" class and assign it to the variable "person".
    val person: Person = new Person("John Doe", 30)

    // Call the "greet" method of the "person" object.
    person.greet()
  }
}
```

**Explanation of the code:**

1. The code starts with a multi-line comment that explains the purpose of the program.

2. It then defines an object named "Main", which is the entry point of the program.

3. Inside the "Main" object, the main method is defined, which is the entry point of the program.

4. The main method prints a welcome message to the console.

5. It then defines a variable named "name" and assigns it the value "John Doe".

6. The value of the "name" variable is printed to the console.

7. A list of numbers is defined and printed to the console.

8. The list of numbers is iterated over and each number is printed to the console.

9. A function named "sum" is defined that takes two numbers as arguments and returns their sum.

10. The "sum" function is called with the numbers 1 and 2 and the result is printed to the console.

11. A class named "Person" is defined with two properties: "name" and "age".

12. A method named "greet" is defined inside the "Person" class that prints a greeting message to the console.

13. An instance of the "Person" class is created and assigned to the variable "person".

14. The "greet" method of the "person" object is called.

15. The program ends.