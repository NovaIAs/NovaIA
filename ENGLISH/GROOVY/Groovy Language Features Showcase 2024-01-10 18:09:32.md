```groovy
// This is a Groovy script that demonstrates the use of various language features, including:
// - Classes and objects
// - Inheritance
// - Polymorphism
// - Generics
// - Closures
// - Regular expressions
// - Metaprogramming

// Define a base class called "Animal"
class Animal {
    String name
    int age

    // Constructor
    Animal(String name, int age) {
        this.name = name
        this.age = age
    }

    // Method to print the animal's name and age
    void printInfo() {
        println "Name: $name, Age: $age"
    }
}

// Define a subclass of "Animal" called "Dog"
class Dog extends Animal {
    String breed

    // Constructor
    Dog(String name, int age, String breed) {
        super(name, age)
        this.breed = breed
    }

    // Override the "printInfo" method to include the breed
    @Override
    void printInfo() {
        super.printInfo()
        println "Breed: $breed"
    }
}

// Define a subclass of "Animal" called "Cat"
class Cat extends Animal {
    String furColor

    // Constructor
    Cat(String name, int age, String furColor) {
        super(name, age)
        this.furColor = furColor
    }

    // Override the "printInfo" method to include the fur color
    @Override
    void printInfo() {
        super.printInfo()
        println "Fur Color: $furColor"
    }
}

// Define a generic class called "AnimalList" that can hold a list of animals
class AnimalList<T extends Animal> {
    List<T> animals

    // Constructor
    AnimalList() {
        animals = new ArrayList<>()
    }

    // Method to add an animal to the list
    void addAnimal(T animal) {
        animals.add(animal)
    }

    // Method to print the information of all animals in the list
    void printAllInfo() {
        for (T animal in animals) {
            animal.printInfo()
        }
    }
}

// Define a closure that takes a string as input and returns a list of strings
def splitString = { String str -> str.split(",") }

// Define a regular expression to match email addresses
def emailRegex = /\S+@\S+\.\S+/

// Define a metaclass for the "String" class that adds a new method called "reverse"
String.metaClass.reverse = { -> delegate.reverse() }

// Create a list of animals
def animalList = new AnimalList<>()
animalList.addAnimal(new Dog("Buddy", 3, "Golden Retriever"))
animalList.addAnimal(new Cat("Kitty", 2, "Persian"))

// Print the information of all animals in the list
animalList.printAllInfo()

// Split a string using the closure
def str = "apple,banana,cherry"
def strList = splitString(str)
println "Split String: $strList"

// Validate an email address using the regular expression
def email = "john.doe@example.com"
if (emailRegex.matches(email)) {
    println "Valid Email Address"
} else {
    println "Invalid Email Address"
}

// Reverse a string using the metaclass method
def reversedStr = "Hello World".reverse()
println "Reversed String: $reversedStr"
```

Explanation:

1. Classes and Objects: We define three classes: Animal, Dog, and Cat, representing different types of animals. Each class has its own properties and methods.

2. Inheritance: Dog and Cat classes inherit from the Animal class, inheriting its properties and methods.

3. Polymorphism: The printInfo() method is overridden in Dog and Cat classes, providing different implementations for the same method.

4. Generics: The AnimalList class is a generic class that can hold a list of animals of any type that extends the Animal class.

5. Closures: A closure is defined to split a string into a list of strings. Closures are anonymous functions that can be passed around and executed.

6. Regular Expressions: A regular expression is used to validate email addresses. Regular expressions are used to match patterns in text.

7. Metaprogramming: The metaclass for the String class is modified to add a new method called reverse(). Metaprogramming allows you to modify the behavior of existing classes at runtime.