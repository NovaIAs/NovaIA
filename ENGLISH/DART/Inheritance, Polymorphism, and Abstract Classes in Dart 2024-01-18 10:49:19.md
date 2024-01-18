```dart
/// This is a Dart program to demonstrate the use of inheritance,
/// polymorphism, and abstract classes.

// Define an abstract class called Animal.
abstract class Animal {
  // Define an abstract method called makeSound().
  void makeSound();
}

// Define a class called Dog that inherits from Animal.
class Dog extends Animal {
  // Override the makeSound() method.
  @override
  void makeSound() {
    print('Woof!');
  }
}

// Define a class called Cat that inherits from Animal.
class Cat extends Animal {
  // Override the makeSound() method.
  @override
  void makeSound() {
    print('Meow!');
  }
}

// Define a class called Bird that inherits from Animal.
class Bird extends Animal {
  // Override the makeSound() method.
  @override
  void makeSound() {
    print('Chirp!');
  }
}

// Define a function called main().
void main() {
  // Create an array of animals.
  var animals = [Dog(), Cat(), Bird()];

  // Loop through the array of animals and call the makeSound()
  // method for each animal.
  for (var animal in animals) {
    animal.makeSound();
  }
}

```

Explanation:

This code demonstrates the use of inheritance, polymorphism, and abstract classes in Dart.

1. We define an abstract class called Animal. This class defines an abstract method called makeSound(). Abstract classes are classes that cannot be instantiated directly, but they can be used to define a common interface for subclasses.

2. We define three subclasses of Animal: Dog, Cat, and Bird. These subclasses inherit the makeSound() method from the Animal class.

3. We override the makeSound() method in each subclass to provide a different implementation for each animal. For example, the Dog class overrides the makeSound() method to print "Woof!", the Cat class overrides the makeSound() method to print "Meow!", and the Bird class overrides the makeSound() method to print "Chirp!".

4. We define a function called main().

5. In the main() function, we create an array of animals. The array contains instances of the Dog, Cat, and Bird classes.

6. We loop through the array of animals and call the makeSound() method for each animal. This demonstrates polymorphism, which is the ability for objects of different subclasses to respond to the same method call in different ways.

When you run this code, you will see the following output:

```
Woof!
Meow!
Chirp!
```