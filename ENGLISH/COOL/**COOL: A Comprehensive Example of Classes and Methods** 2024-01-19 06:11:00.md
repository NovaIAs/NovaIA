```cool
module SampleModule {
    class SampleClass {
        class variables:
            classVariable : Int;

        instance variables:
            instanceVariable : Int;

        constructor SampleClass(variable : Int) {
            instanceVariable := variable;
        }

        method multiply(factor : Int) : Int {
            return instanceVariable * factor;
        }

        method printVariable() : Void {
            io.out_int(instanceVariable);
        }

        method staticClassMethod() : Void {
            io.out_int(classVariable);
        }

        method staticClassConstructor() {
            classVariable := 10;
        }
    }

    class MainClass {
        main() : Void {
            SampleClass.staticClassConstructor();
            SampleClass instance1 := new SampleClass(5);
            SampleClass instance2 := new SampleClass(10);
            instance1.printVariable();
            instance2.printVariable();
            SampleClass.staticClassMethod();
            io.out_string("\n");
        }
    }
}
```

Explanation:

1. **Module Definition:**
   - The code starts with the definition of a module named `SampleModule`. Modules are used to group related classes and methods together.

2. **Class Definition:**
   - Inside the module, two classes are defined: `SampleClass` and `MainClass`.
   - `SampleClass` has three sections: class variables, instance variables, and methods.
   - Class variables are shared among all instances of the class, while instance variables are unique to each instance.
   - Methods define the behavior of the class and can be called on its instances.

3. **Constructor:**
   - `SampleClass` has a single constructor that takes an `Int` variable as an argument and assigns it to the `instanceVariable`.

4. **Instance Methods:**
   - `SampleClass` has two instance methods: `multiply` and `printVariable`.
   - `multiply` takes an `Int` as an argument and returns the product of the `instanceVariable` and the argument.
   - `printVariable` prints the value of the `instanceVariable`.

5. **Static Class Methods:**
   - `SampleClass` also has a static class method called `staticClassMethod` that prints the value of the `classVariable`.
   - Static class methods can be called directly on the class without creating an instance.

6. **Static Class Constructor:**
   - `SampleClass` has a static class constructor called `staticClassConstructor` that initializes the `classVariable` to 10.
   - Static class constructors are called automatically when the class is loaded.

7. **Main Class:**
   - `MainClass` is the class that contains the `main` method, which is the entry point of the program.
   - In the `main` method, the `staticClassConstructor` of `SampleClass` is called to initialize the `classVariable`.
   - Two instances of `SampleClass` are created, and their `printVariable` methods are called to print the values of their `instanceVariable`.
   - Finally, the `staticClassMethod` of `SampleClass` is called to print the value of the `classVariable`.

This code demonstrates the use of classes, instance variables, methods, static class methods, static class constructors, and the `main` method in the COOL programming language.