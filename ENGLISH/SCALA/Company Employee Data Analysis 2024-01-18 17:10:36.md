```scala
// Define a data class to represent a person.
case class Person(name: String, age: Int)

// Define a companion object for the Person class.
object Person {
  // Define a method to create a Person from a string.
  def fromString(s: String): Person = {
    val parts = s.split(",")
    Person(parts(0), parts(1).toInt)
  }

  // Define a method to create a list of Person objects from a string.
  def fromStringList(s: String): List[Person] = {
    s.split(",").map(fromString)
  }
}

// Define a class to represent a company.
class Company(val name: String, val employees: List[Person]) {
  // Define a method to get the average age of the employees.
  def averageAge: Double = {
    employees.map(_.age).sum / employees.size
  }

  // Define a method to get the oldest employee.
  def oldestEmployee: Person = {
    employees.maxBy(_.age)
  }

  // Define a method to get the youngest employee.
  def youngestEmployee: Person = {
    employees.minBy(_.age)
  }
}

// Define a companion object for the Company class.
object Company {
  // Define a method to create a Company from a string.
  def fromString(s: String): Company = {
    val parts = s.split(";")
    Company(parts(0), Person.fromStringList(parts(1)))
  }

  // Define a method to create a list of Company objects from a string.
  def fromStringList(s: String): List[Company] = {
    s.split(",").map(fromString)
  }
}

// Define a main() method to test the code.
def main(args: Array[String]): Unit = {
  // Create a list of Person objects.
  val people = List(
    Person("John Doe", 30),
    Person("Jane Doe", 25),
    Person("Bob Smith", 40)
  )

  // Create a Company object.
  val company = Company("Acme Corporation", people)

  // Print the average age of the employees.
  println(s"Average age: ${company.averageAge}")

  // Print the oldest employee.
  println(s"Oldest employee: ${company.oldestEmployee}")

  // Print the youngest employee.
  println(s"Youngest employee: ${company.youngestEmployee}")
}
```

Explanation:

1. We define a data class `Person` to represent a person with two fields: `name` and `age`.
2. We define a companion object `Person` for the `Person` class, which provides a method `fromString` to create a `Person` object from a string and a method `fromStringList` to create a list of `Person` objects from a string.
3. We define a class `Company` to represent a company with two fields: `name` and `employees`.
4. We define a companion object `Company` for the `Company` class, which provides a method `fromString` to create a `Company` object from a string and a method `fromStringList` to create a list of `Company` objects from a string.
5. We define a main method to test the code.
6. We create a list of `Person` objects and a `Company` object.
7. We use the methods defined in the `Company` class to print the average age, oldest employee, and youngest employee of the company.