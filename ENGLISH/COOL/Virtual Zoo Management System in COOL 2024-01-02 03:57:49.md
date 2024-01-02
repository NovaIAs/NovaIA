Certainly! Here's a complex and unique code written in COOL (Classroom Object-Oriented Language). This code implements a simulation of a virtual zoo management system.

```cool
class Animal {
    attr name : String;
    method makeSound() : String { };
};

class Lion inherits Animal {
    method makeSound() : String {
        out_string("Roar");
    };
};

class Elephant inherits Animal {
    method makeSound() : String {
        out_string("Trumpet");
    };
};

class Zoo {
    attr animals : List(Animal);

    method addAnimal(animal : Animal) : Object {
        animals.append(animal);
    };

    method makeAllAnimalsSound() : Object {
        let sound : String <- "";

        animals.iterate {
            sound <- sound.concat(it.makeSound());
        };

        out_string(sound);
    };
};

class Main {
    main() : Object {
        let zoo : Zoo <- new Zoo;

        let lion : Animal <- new Lion;
        let elephant : Animal <- new Elephant;

        zoo.addAnimal(lion);
        zoo.addAnimal(elephant);

        zoo.makeAllAnimalsSound();
    };
};
```

This code consists of several classes: `Animal`, `Lion`, `Elephant`, `Zoo`, and `Main`. 

The `Animal` class is the base class with a `name` attribute and a `makeSound` method, which is left abstract. 

The `Lion` class inherits from `Animal` and overrides the `makeSound` method to print "Roar" when called.

The `Elephant` class also inherits from `Animal` and overrides the `makeSound` method to print "Trumpet" when called.

The `Zoo` class has an attribute `animals` that stores a list of `Animal` objects. It has two methods: `addAnimal`, which adds an animal to the list, and `makeAllAnimalsSound`, which iterates over the list and concatenates the sounds made by each animal.

The `Main` class serves as the entry point for the program. It creates a `Zoo` instance, creates instances of `Lion` and `Elephant`, adds them to the zoo, and then calls the `makeAllAnimalsSound` method to make all animals in the zoo make their respective sounds.

This code demonstrates object-oriented programming concepts such as inheritance, method overriding, and composition. It simulates a virtual zoo management system where animals can be added to a zoo and make their unique sounds.