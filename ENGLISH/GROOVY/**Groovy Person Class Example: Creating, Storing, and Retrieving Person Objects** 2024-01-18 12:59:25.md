```groovy
// Define a class to represent a Person
class Person {
    String name
    int age
    String city

    // Define a constructor for the Person class
    Person(String name, int age, String city) {
        this.name = name
        this.age = age
        this.city = city
    }

    // Define a method to print the Person's information
    def printInfo() {
        println "Name: $name"
        println "Age: $age"
        println "City: $city"
    }
}

// Define a list of Person objects
def people = [
    new Person("John Doe", 30, "New York"),
    new Person("Jane Smith", 25, "Los Angeles"),
    new Person("Michael Jones", 40, "Chicago")
]

// Iterate over the list of Person objects and print their information
for (person in people) {
    person.printInfo()
    println ""
}

// Define a Map to store the Person objects by their names
def peopleByName = [:]

// Iterate over the list of Person objects and add them to the Map
for (person in people) {
    peopleByName[person.name] = person
}

// Print the names of the Person objects in the Map
println "People by name:"
for (name in peopleByName.keySet()) {
    println name
}

// Get the Person object for a given name
def johnDoe = peopleByName["John Doe"]

// Print the information for the Person object
println "John Doe's information:"
johnDoe.printInfo()
```
Explanation:

1. **Class Definition**: We define a class named `Person` to represent a person with attributes like name, age, and city.

2. **Constructor**: We define a constructor for the `Person` class that takes three parameters: name, age, and city, and initializes the attributes of the `Person` object.

3. **Method Definition**: We define a method called `printInfo()` in the `Person` class that prints the information of the `Person` object, including name, age, and city.

4. **Create List of Person Objects**: We create a list named `people` that contains three `Person` objects with different names, ages, and cities.

5. **Iterate and Print Person Information**: We iterate over the list of `people` and call the `printInfo()` method on each `Person` object to print their information.

6. **Create Map of Person Objects by Name**: We create a Map named `peopleByName` to store the `Person` objects by their names.

7. **Add Person Objects to Map**: We iterate over the list of `people` and add each `Person` object to the `peopleByName` Map using their names as keys.

8. **Print Names of Person Objects in Map**: We iterate over the keys of the `peopleByName` Map and print the names of the `Person` objects.

9. **Get Person Object by Name**: We retrieve the `Person` object for a given name, in this case, "John Doe", from the `peopleByName` Map.

10. **Print Information for Specific Person**: We call the `printInfo()` method on the `Person` object retrieved by name to print its information.