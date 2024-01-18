```scala
// Define a trait called `Animal` with an abstract method `speak()`
trait Animal {
  def speak(): String
}

// Define a class called `Dog` that extends the `Animal` trait and overrides the `speak()` method
class Dog extends Animal {
  override def speak(): String = "Woof!"
}

// Define a class called `Cat` that extends the `Animal` trait and overrides the `speak()` method
class Cat extends Animal {
  override def speak(): String = "Meow!"
}

// Define a class called `Cow` that extends the `Animal` trait and overrides the `speak()` method
class Cow extends Animal {
  override def speak(): String = "Moo!"
}

// Define a class called `Snake` that extends the `Animal` trait and overrides the `speak()` method
class Snake extends Animal {
  override def speak(): String = "Hiss!"
}

// Define a class called `Lion` that extends the `Animal` trait and overrides the `speak()` method
class Lion extends Animal {
  override def speak(): String = "Roar!"
}

// Define an object called `AnimalSounds`
object AnimalSounds {

  // Define a method called `makeAllAnimalsSpeak()` that takes a list of animals and prints the sound each animal makes
  def makeAllAnimalsSpeak(animals: List[Animal]): Unit = {
    for (animal <- animals) {
      println(animal.speak())
    }
  }

  // Define a method called `main()` that creates a list of animals and calls the `makeAllAnimalsSpeak()` method
  def main(args: Array[String]): Unit = {
    val animals = List(new Dog(), new Cat(), new Cow(), new Snake(), new Lion())
    makeAllAnimalsSpeak(animals)
  }
}
```

This code defines a trait called `Animal` with an abstract method `speak()`. It then defines five classes (`Dog`, `Cat`, `Cow`, `Snake`, and `Lion`) that extend the `Animal` trait and override the `speak()` method to return different sounds.

The code also defines an object called `AnimalSounds` that contains a method called `makeAllAnimalsSpeak()` that takes a list of animals and prints the sound each animal makes.

Finally, the code defines a `main()` method that creates a list of animals and calls the `makeAllAnimalsSpeak()` method.

When the code is run, it will print the following output:

```
Woof!
Meow!
Moo!
Hiss!
Roar!
```