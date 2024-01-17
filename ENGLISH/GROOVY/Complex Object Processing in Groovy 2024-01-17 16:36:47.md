```groovy
// Define a class to represent a complex object
class ComplexObject {

    // Define properties for the object
    String name
    int age
    List<String> hobbies

    // Define a constructor for the object
    ComplexObject(String name, int age, List<String> hobbies) {
        this.name = name
        this.age = age
        this.hobbies = hobbies
    }

    // Define a method to print the object's properties
    void printProperties() {
        println "Name: $name"
        println "Age: $age"
        println "Hobbies: $hobbies"
    }
}

// Create a list of complex objects
List<ComplexObject> complexObjects = []

// Add some complex objects to the list
complexObjects << new ComplexObject("John Doe", 30, ["hiking", "fishing", "camping"])
complexObjects << new ComplexObject("Jane Smith", 25, ["reading", "writing", "painting"])
complexObjects << new ComplexObject("Bill Jones", 40, ["golfing", "tennis", "swimming"])

// Print the properties of each complex object in the list
complexObjects.each { complexObject ->
    complexObject.printProperties()
    println()
}

// Define a method to find the oldest complex object in the list
ComplexObject findOldestObject(List<ComplexObject> complexObjects) {
    ComplexObject oldestObject = null
    for (complexObject in complexObjects) {
        if (oldestObject == null || complexObject.age > oldestObject.age) {
            oldestObject = complexObject
        }
    }
    return oldestObject
}

// Find the oldest complex object in the list
ComplexObject oldestObject = findOldestObject(complexObjects)

// Print the properties of the oldest complex object
println "Oldest object:"
oldestObject.printProperties()

// Define a method to find the most popular hobby among the complex objects in the list
String findMostPopularHobby(List<ComplexObject> complexObjects) {
    Map<String, Integer> hobbyCounts = [:]
    for (complexObject in complexObjects) {
        for (hobby in complexObject.hobbies) {
            hobbyCounts[hobby] = hobbyCounts[hobby] ?: 0
            hobbyCounts[hobby]++
        }
    }
    String mostPopularHobby = null
    int maxCount = 0
    for (hobby in hobbyCounts) {
        if (hobby.value > maxCount) {
            mostPopularHobby = hobby.key
            maxCount = hobby.value
        }
    }
    return mostPopularHobby
}

// Find the most popular hobby among the complex objects in the list
String mostPopularHobby = findMostPopularHobby(complexObjects)

// Print the most popular hobby
println "Most popular hobby: $mostPopularHobby"
```

This code defines a class called `ComplexObject` to represent a complex object with properties for the object's name, age, and a list of hobbies. The code then creates a list of complex objects and prints the properties of each object in the list. The code also defines methods to find the oldest complex object in the list and the most popular hobby among the complex objects in the list. The methods are then called and the results are printed.