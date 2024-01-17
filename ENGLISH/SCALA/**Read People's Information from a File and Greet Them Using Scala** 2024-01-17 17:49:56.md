```scala
import scala.io.Source

class Person(val name: String, val age: Int) {
  def greet() = println(s"Hello, my name is $name and I am $age years old.")
}

object Person {
  def main(args: Array[String]): Unit = {
    val people = Source.fromFile("people.txt").getLines().map(_.split(",")).map(p => new Person(p(0), p(1).toInt)).toList

    people.foreach(_.greet())
  }
}
```

**Explanation:**

1. **Importing the scala.io.Source Class:**

   ```scala
   import scala.io.Source
   ```

   This line imports the `scala.io.Source` class, which provides methods for reading data from files and URLs.


2. **Defining the Person Class:**

   ```scala
   class Person(val name: String, val age: Int) {
     def greet() = println(s"Hello, my name is $name and I am $age years old.")
   }
   ```

   This code defines a `Person` class with two properties: `name` and `age`. The constructor takes these two arguments and initializes the properties. It also defines a `greet()` method that prints a greeting message using the values of the `name` and `age` properties.


3. **Defining the Companion Object for the Person Class:**

   ```scala
   object Person {
     def main(args: Array[String]): Unit = {
       // Code here
     }
   }
   ```

   In Scala, a companion object is a singleton object that is associated with a class. It can access the private members of the class and is often used to define factory methods or other utility methods related to the class.

   In this case, the companion object for the `Person` class defines a `main` method, which is the entry point of the program.


4. **Reading Data from a File:**

   ```scala
   val people = Source.fromFile("people.txt").getLines().map(_.split(",")).map(p => new Person(p(0), p(1).toInt)).toList
   ```

   This line of code reads the contents of the "people.txt" file using the `Source.fromFile` method. It then uses the `getLines()` method to split the file contents into individual lines.

   Next, it uses the `map` method to transform each line into an array of strings by splitting it at the comma character.

   Finally, it uses another `map` method to create a list of `Person` objects from the array of strings. Each `Person` object is created by passing the first element of the array as the name and the second element as the age.


5. **Printing Greetings for Each Person:**

   ```scala
   people.foreach(_.greet())
   ```

   This line of code uses the `foreach` method to iterate over the `people` list and call the `greet()` method on each `Person` object. This will print a greeting message for each person to the console.