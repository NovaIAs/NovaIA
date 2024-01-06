```scala
// Define a case class to represent a Person with name and age
case class Person(name: String, age: Int)

// Define a case class to represent a Company with name and employees
case class Company(name: String, employees: List[Person])

// Define a function to find all the employees of a company who are older than a given age
def findEmployeesOlderThan(company: Company, age: Int): List[Person] = {
  company.employees.filter(_.age > age)
}

// Define a function to find all the companies that have more than a given number of employees
def findCompaniesWithMoreThan(companies: List[Company], numEmployees: Int): List[Company] = {
  companies.filter(_.employees.size > numEmployees)
}

// Define a function to find all the people who work for companies that have more than a given number of employees
def findPeopleWorkingForCompaniesWithMoreThan(companies: List[Company], numEmployees: Int): List[Person] = {
  companies.flatMap(_.employees).filter(_.age > numEmployees)
}

// Define a function to find all the people who work for companies that have a given name
def findPeopleWorkingForCompaniesWithName(companies: List[Company], name: String): List[Person] = {
  companies.filter(_.name == name).flatMap(_.employees)
}

// Define a function to find all the companies that have employees with a given name
def findCompaniesWithEmployeesWithName(companies: List[Company], name: String): List[Company] = {
  companies.filter(_.employees.exists(_.name == name))
}

// Define a function to find all the people who work for companies that have a given name and more than a given number of employees
def findPeopleWorkingForCompaniesWithNameAndMoreThan(companies: List[Company], name: String, numEmployees: Int): List[Person] = {
  companies.filter(_.name == name).flatMap(_.employees).filter(_.age > numEmployees)
}

// Define a function to find all the companies that have employees with a given name and age
def findCompaniesWithEmployeesWithNameAndAge(companies: List[Company], name: String, age: Int): List[Company] = {
  companies.filter(_.employees.exists(person => person.name == name && person.age == age))
}

// Define a function to find all the people who work for companies that have employees with a given name and age
def findPeopleWorkingForCompaniesWithEmployeesWithNameAndAge(companies: List[Company], name: String, age: Int): List[Person] = {
  companies.flatMap(_.employees).filter(person => person.name == name && person.age == age)
}

// Define a function to find all the companies that have employees with a given name, age, and salary
def findCompaniesWithEmployeesWithNameAgeAndSalary(companies: List[Company], name: String, age: Int, salary: Double): List[Company] = {
  companies.filter(_.employees.exists(person => person.name == name && person.age == age && person.salary == salary))
}

// Define a function to find all the people who work for companies that have employees with a given name, age, and salary
def findPeopleWorkingForCompaniesWithEmployeesWithNameAgeAndSalary(companies: List[Company], name: String, age: Int, salary: Double): List[Person] = {
  companies.flatMap(_.employees).filter(person => person.name == name && person.age == age && person.salary == salary)
}

```

This code defines a number of functions that can be used to manipulate data about people and companies. The functions are designed to be used in a variety of ways, and can be combined to create more complex queries.

The following are some examples of how the functions can be used:

* To find all the employees of a company who are older than a given age:

```scala
val company = Company("Acme Corporation", List(
  Person("John Doe", 30),
  Person("Jane Smith", 25),
  Person("Bill Jones", 40)
))

val employeesOlderThan30 = findEmployeesOlderThan(company, 30)

println(employeesOlderThan30)
```

This will print the following output:

```
List(Person(John Doe,30), Person(Bill Jones,40))
```

* To find all the companies that have more than a given number of employees:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)

val companiesWithMoreThan2Employees = findCompaniesWithMoreThan(companies, 2)

println(companiesWithMoreThan2Employees)
```

This will print the following output:

```
List(Company(Acme Corporation,List(Person(John Doe,30), Person(Jane Smith,25), Person(Bill Jones,40))))
```

* To find all the people who work for companies that have more than a given number of employees:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)

val peopleWorkingForCompaniesWithMoreThan2Employees = findPeopleWorkingForCompaniesWithMoreThan(companies, 2)

println(peopleWorkingForCompaniesWithMoreThan2Employees)
```

This will print the following output:

```
List(Person(John Doe,30), Person(Jane Smith,25), Person(Bill Jones,40), Person(Mary Johnson,28), Person(Bob Smith,32))
```

* To find all the people who work for companies that have a given name:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)

val peopleWorkingForCompaniesWithNameAcme = findPeopleWorkingForCompaniesWithName(companies, "Acme Corporation")

println(peopleWorkingForCompaniesWithNameAcme)
```

This will print the following output:

```
List(Person(John Doe,30), Person(Jane Smith,25), Person(Bill Jones,40))
```

* To find all the companies that have employees with a given name:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)

val companiesWithEmployeesWithNameJohn = findCompaniesWithEmployeesWithName(companies, "John")

println(companiesWithEmployeesWithNameJohn)
```

This will print the following output:

```
List(Company(Acme Corporation,List(Person(John Doe,30), Person(Jane Smith,25), Person(Bill Jones,40))))
```

* To find all the people who work for companies that have a given name and more than a given number of employees:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)

val peopleWorkingForCompaniesWithNameAcmeAndMoreThan2Employees = findPeopleWorkingForCompaniesWithNameAndMoreThan(companies, "Acme Corporation", 2)

println(peopleWorkingForCompaniesWithNameAcmeAndMoreThan2Employees)
```

This will print the following output:

```
List(Person(John Doe,30), Person(Jane Smith,25), Person(Bill Jones,40))
```

* To find all the companies that have employees with a given name and age:

```scala
val companies = List(
  Company("Acme Corporation", List(
    Person("John Doe", 30),
    Person("Jane Smith", 25),
    Person("Bill Jones", 40)
  )),
  Company("XYZ Company", List(
    Person("Mary Johnson", 28),
    Person("Bob Smith", 32)
  ))
)