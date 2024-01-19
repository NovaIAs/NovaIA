```groovy
// Define a class to represent a Person
class Person {
    String name
    int age
    String city

    // Define a constructor to initialize the properties
    Person(String name, int age, String city) {
        this.name = name
        this.age = age
        this.city = city
    }

    // Define a method to print the person's information
    def printInfo() {
        println "Name: $name"
        println "Age: $age"
        println "City: $city"
    }
}

// Define a list of persons
def persons = [
    new Person("John Doe", 30, "New York"),
    new Person("Jane Smith", 25, "London"),
    new Person("Michael Jones", 40, "Paris")
]

// Print the information of each person
persons.each { person ->
    person.printInfo()
    println()
}

// Define a closure to compare two persons based on their age
def compareByAge = { Person p1, Person p2 -> p1.age <=> p2.age }

// Sort the list of persons by age using the closure
def sortedPersons = persons.sort(compareByAge)

// Print the information of each sorted person
sortedPersons.each { person ->
    person.printInfo()
    println()
}

// Define a closure to filter the list of persons based on their city
def filterByCity = { Person p -> p.city == "New York" }

// Filter the list of persons by city using the closure
def filteredPersons = persons.findAll(filterByCity)

// Print the information of each filtered person
filteredPersons.each { person ->
    person.printInfo()
    println()
}

// Define a closure to transform each person's name to uppercase
def transformName = { Person p -> p.name.toUpperCase() }

// Transform the list of persons' names using the closure
def transformedNames = persons.collect(transformName)

// Print the transformed names
println "Transformed Names:"
transformedNames.each { name ->
    println "$name"
}
```