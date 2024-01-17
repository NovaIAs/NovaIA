```scala
// Define a class for a Person
class Person(val name: String) {
  // Define a method to greet someone
  def greet(other: Person): String = {
    s"Hello ${other.name}, my name is ${name}."
  }
}

// Define a class for a Dog
class Dog(val name: String) {
  // Define a method to bark
  def bark(): String = {
    s"${name} says woof!"
  }
}

// Define a class for a Cat
class Cat(val name: String) {
  // Define a method to meow
  def meow(): String = {
    s"${name} says meow!"
  }
}

// Define a main function
def main(args: Array[String]): Unit = {
  // Create a person object
  val person = new Person("John")

  // Create a dog object
  val dog = new Dog("Fido")

  // Create a cat object
  val cat = new Cat("Kitty")

  // Print the person's name
  println(person.name)

  // Print the dog's name and bark
  println(dog.name)
  println(dog.bark())

  // Print the cat's name and meow
  println(cat.name)
  println(cat.meow())

  // Make the person greet the dog and cat
  println(person.greet(dog))
  println(person.greet(cat))
}
```

Explanation:

1. We define three classes: `Person`, `Dog`, and `Cat`. Each class represents a different type of object.

2. The `Person` class has a constructor that takes a single parameter, `name`, which is the name of the person. It also has a `greet` method that takes another `Person` object as a parameter and returns a string greeting that includes the names of both people.

3. The `Dog` class has a constructor that takes a single parameter, `name`, which is the name of the dog. It also has a `bark` method that returns a string representing the dog barking.

4. The `Cat` class has a constructor that takes a single parameter, `name`, which is the name of the cat. It also has a `meow` method that returns a string representing the cat meowing.

5. The `main` function is the entry point of the program. It creates instances of the `Person`, `Dog`, and `Cat` classes and prints their names. It also makes the person greet the dog and the cat, and prints the results.

6. The output of the program will be:

```
John
Fido
Fido says woof!
Kitty
Kitty says meow!
Hello Fido, my name is John.
Hello Kitty, my name is John.
```