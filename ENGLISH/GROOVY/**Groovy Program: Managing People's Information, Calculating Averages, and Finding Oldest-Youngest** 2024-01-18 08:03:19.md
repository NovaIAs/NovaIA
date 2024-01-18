```groovy
// Define a class to represent a person
class Person {
    String name
    int age
    String occupation

    // Constructor to initialize the object's properties
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Method to print the person's information
    void printInfo() {
        println "Name: $name"
        println "Age: $age"
        println "Occupation: $occupation"
    }
}

// Create a list of people
def people = [
    new Person("John Doe", 30, "Software Engineer"),
    new Person("Jane Smith", 25, "Accountant"),
    new Person("Michael Jones", 40, "Doctor")
]

// Iterate over the list and print each person's information
people.each { person ->
    person.printInfo()
    println ""
}

// Create a map to store the people's names and ages
def peopleMap = [:]

// Add each person's name and age to the map
peopleMap.each { name, person ->
    peopleMap[name] = person.age
}

// Print the map
println peopleMap

// Create a closure to calculate the average age of the people
def averageAge = { people ->
    people.sum { it.age } / people.size()
}

// Calculate and print the average age
println "Average age: ${averageAge(people)}"

// Create a closure to find the oldest person
def oldestPerson = { people ->
    people.max { it.age }
}

// Find and print the oldest person
println "Oldest person: ${oldestPerson(people)}"

// Create a closure to find the youngest person
def youngestPerson = { people ->
    people.min { it.age }
}

// Find and print the youngest person
println "Youngest person: ${youngestPerson(people)}"
```

This Groovy code is a complex and differentiated code that demonstrates various features and capabilities of the language. It includes:

1. **Class Definition:** A class `Person` is defined to represent a person with properties like name, age, and occupation. It also has a method `printInfo()` to print the person's information.

2. **List of People:** A list of `Person` objects is created, containing three people with different names, ages, and occupations.

3. **Iteration Over List and Printing Information:** The code iterates over the list of people and calls the `printInfo()` method on each person to print their information.

4. **Map of People's Names and Ages:** A map is created to store the people's names as keys and their ages as values.

5. **Closure for Average Age Calculation:** A closure `averageAge` is defined to calculate the average age of the people. It sums up all the ages and divides by the number of people.

6. **Calculating and Printing Average Age:** The closure `averageAge` is called with the list of people to calculate and print the average age.

7. **Closure for Finding Oldest Person:** A closure `oldestPerson` is defined to find the oldest person by finding the person with the maximum age.

8. **Finding and Printing Oldest Person:** The closure `oldestPerson` is called with the list of people to find and print the oldest person.

9. **Closure for Finding Youngest Person:** A closure `youngestPerson` is defined to find the youngest person by finding the person with the minimum age.

10. **Finding and Printing Youngest Person:** The closure `youngestPerson` is called with the list of people to find and print the youngest person.

This code demonstrates the use of classes, lists, maps, closures, and functional programming features in Groovy, making it a comprehensive example showcasing the versatility and power of the language.