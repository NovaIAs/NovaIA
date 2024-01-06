```scala
// Define a case class to represent a person
case class Person(name: String, age: Int) {
  // Define a method to return the person's name
  def getName: String = name

  // Define a method to return the person's age
  def getAge: Int = age

  // Define a method to return a string representation of the person
  override def toString: String = s"Person(name=$name, age=$age)"
}

// Define a class to represent a company
class Company(name: String) {
  // Define a list to store the company's employees
  private var employees: List[Person] = List()

  // Define a method to add an employee to the company
  def addEmployee(employee: Person): Unit = employees = employee :: employees

  // Define a method to get the company's employees
  def getEmployees: List[Person] = employees

  // Define a method to return a string representation of the company
  override def toString: String = s"Company(name=$name, employees=$employees)"
}

// Define an object to create a company and add employees
object CompanyExample {
  // Define a main method to run the program
  def main(args: Array[String]): Unit = {
    // Create a new company
    val company = new Company("Acme Corporation")

    // Add some employees to the company
    company.addEmployee(Person("John Doe", 30))
    company.addEmployee(Person("Jane Doe", 25))
    company.addEmployee(Person("Bill Smith", 40))

    // Print the company's name and employees
    println(s"Company: ${company.name}")
    println("Employees:")
    company.getEmployees.foreach(println)
  }
}
```

Explanation:

1. We define a case class `Person` to represent a person with fields for name and age, and methods to get the name, age, and a string representation of the person.

2. We define a class `Company` to represent a company with a name and a list of employees. It has methods to add employees, get the list of employees, and return a string representation of the company.

3. In the `CompanyExample` object, we define a `main` method as the entry point of the program.

4. Inside the `main` method, we create a new `Company` object named `company`.

5. We add three `Person` objects to the `company` object using the `addEmployee` method.

6. We print the company's name and its employees using `println`.

This code demonstrates the use of case classes, classes, objects, and methods in Scala to create and manipulate data structures.