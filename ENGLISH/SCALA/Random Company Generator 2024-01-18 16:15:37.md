```scala
// Define a class to represent a Person with various attributes
class Person(val name: String, val age: Int, val occupation: String) {

  // Function to display the person's details
  def display(): Unit = {
    println(s"Name: $name")
    println(s"Age: $age")
    println(s"Occupation: $occupation")
  }
}

// Define a case class to represent a Company with a name, revenue, and list of employees
case class Company(name: String, revenue: Double, employees: List[Person]) {

  // Function to calculate the total salary of all employees in the company
  def calculateTotalSalary(): Double = {
    employees.map(_.age * 1000).sum
  }

  // Function to display the company's details
  def display(): Unit = {
    println(s"Name: $name")
    println(s"Revenue: $revenue")
    println(s"Employees:")
    employees.foreach(_.display())
  }
}

// Define a companion object for the Company class to provide factory methods
object Company {

  // Factory method to create a company from a name, revenue, and list of employee names and ages
  def apply(name: String, revenue: Double, employeeNamesAndAges: List[(String, Int)]): Company = {
    val employees = employeeNamesAndAges.map { case (name, age) => Person(name, age, "Employee") }
    new Company(name, revenue, employees)
  }
}

// Define a function to generate a list of random companies
def generateRandomCompanies(numCompanies: Int): List[Company] = {
  (1 to numCompanies).map { _ =>
    val name = s"Company ${Random.nextInt(100)}"
    val revenue = Random.nextDouble() * 1000000
    val numEmployees = Random.nextInt(10) + 1
    val employeeNamesAndAges = (1 to numEmployees).map { _ =>
      (Random.alphanumeric.take(10).mkString, Random.nextInt(65) + 18)
    }
    Company(name, revenue, employeeNamesAndAges)
  }
}

// Generate a list of 10 random companies
val companies = generateRandomCompanies(10)

// Display the details of each company
companies.foreach(_.display())
```

**Explanation:**

1. We define a class `Person` to represent a person with attributes `name`, `age`, and `occupation`. It has a function `display()` to display the person's details.

2. We define a case class `Company` to represent a company with attributes `name`, `revenue`, and a list of `Person` objects representing its employees. It has functions `calculateTotalSalary()` to calculate the total salary of all employees and `display()` to display the company's details.

3. We provide a companion object for the `Company` class to define a factory method `apply` that creates a company from a name, revenue, and a list of employee names and ages.

4. We define a function `generateRandomCompanies` that generates a list of random companies with random names, revenues, and employees.

5. We generate a list of 10 random companies and display the details of each company.

This code demonstrates the use of classes, case classes, companion objects, factory methods, and functional programming features such as `map`, `foreach`, and `sum` in Scala. It generates and displays a list of random companies with their employees and total salary.