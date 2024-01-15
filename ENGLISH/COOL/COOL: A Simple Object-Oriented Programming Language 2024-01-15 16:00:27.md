```cool
class Main inherits IO {
    main() : Object {
        (new A()).greet();
        (new B()).greet();
    }
}

class A inherits Object {
    greet() : Object {
        out_string("Hello from A!\n");
    }
}

class B inherits Object {
    greet() : Object {
        out_string("Hello from B!\n");
    }
}
```

This code defines two classes, `A` and `B`, both of which inherit from the `Object` class. The `Main` class also inherits from the `Object` class and contains the `main` method, which is the entry point for the program.

The `greet` method in the `A` class prints "Hello from A!" to the console. The `greet` method in the `B` class prints "Hello from B!" to the console.

The `main` method in the `Main` class creates an instance of the `A` class and calls the `greet` method on it. It then creates an instance of the `B` class and calls the `greet` method on it.

This code demonstrates the basic principles of object-oriented programming in COOL, including inheritance and polymorphism.