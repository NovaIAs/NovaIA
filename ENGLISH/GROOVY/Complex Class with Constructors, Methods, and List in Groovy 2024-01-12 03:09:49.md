```groovy
// Define a complex class with multiple constructors and methods
class ComplexClass {
    // Define instance variables
    private String name
    private int age
    private List<String> hobbies

    // Define constructors
    // Default constructor
    ComplexClass() {
        this.name = "Default Name"
        this.age = 0
        this.hobbies = []
    }

    // Constructor with parameters
    ComplexClass(String name, int age, List<String> hobbies) {
        this.name = name
        this.age = age
        this.hobbies = hobbies
    }

    // Define methods
    // Getter for name
    String getName() {
        return name
    }

    // Setter for name
    void setName(String name) {
        this.name = name
    }

    // Getter for age
    int getAge() {
        return age
    }

    // Setter for age
    void setAge(int age) {
        this.age = age
    }

    // Getter for hobbies
    List<String> getHobbies() {
        return hobbies
    }

    // Setter for hobbies
    void setHobbies(List<String> hobbies) {
        this.hobbies = hobbies
    }

    // Method to add a hobby
    void addHobby(String hobby) {
        hobbies.add(hobby)
    }

    // Method to remove a hobby
    void removeHobby(String hobby) {
        hobbies.remove(hobby)
    }

    // Method to print the details of the object
    void printDetails() {
        println("Name: $name")
        println("Age: $age")
        println("Hobbies: $hobbies")
    }
}

// Create an instance of the ComplexClass
ComplexClass person = new ComplexClass("John Doe", 30, ["Reading", "Hiking", "Coding"])

// Print the details of the object
person.printDetails()

// Add a new hobby
person.addHobby("Traveling")

// Print the details of the object again
person.printDetails()
```

Explanation:

1. We define a complex class called `ComplexClass` with instance variables `name`, `age`, and `hobbies`.

2. We define two constructors: a default constructor and a constructor with parameters. The constructor with parameters takes `name`, `age`, and `hobbies` as arguments and initializes the instance variables accordingly.

3. We define getter and setter methods for each instance variable to allow access and modification of their values.

4. We define additional methods like `addHobby`, `removeHobby`, and `printDetails` to perform specific operations on the object.

5. We create an instance of the `ComplexClass` called `person` and initialize it with the values "John Doe", 30, and a list of hobbies ["Reading", "Hiking", "Coding"].

6. We use the `printDetails` method to print the details of the `person` object.

7. We add a new hobby "Traveling" to the `person` object using the `addHobby` method.

8. We print the details of the `person` object again to show the updated hobbies list.

This code demonstrates the creation and usage of a complex class in Groovy, including the use of constructors, methods, and instance variables. It also illustrates the use of a list to store a collection of values.