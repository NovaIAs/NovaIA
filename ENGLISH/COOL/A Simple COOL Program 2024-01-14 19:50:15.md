```cool
-- Main module
CLASS Main {
    -- Main method
    MAIN() : IO -> IO {
        -- Create a new object of class A
        a <- new A();

        -- Call the method 'foo' on the object 'a'
        a.foo();

        -- Create a new object of class B
        b <- new B();

        -- Call the method 'bar' on the object 'b'
        b.bar();
    }
}

-- Class A
CLASS A {
    -- Method 'foo'
    foo() : IO -> IO {
        -- Print a message to the console
        IO.printString("Hello from class A!");

        -- Return nothing
        return;
    }
}

-- Class B
CLASS B {
    -- Method 'bar'
    bar() : IO -> IO {
        -- Print a message to the console
        IO.printString("Hello from class B!");

        -- Return nothing
        return;
    }
}
```

**Explanation:**

This code defines a simple program in the COOL programming language. The program consists of three classes: `Main`, `A`, and `B`. The `Main` class contains the main method, which is the entry point of the program. The main method creates objects of classes `A` and `B` and calls methods on those objects.

The `A` and `B` classes each define a single method, `foo` and `bar`, respectively. These methods simply print a message to the console.

The `IO` class provides methods for input and output operations. The `printString` method is used to print a string to the console.

To run this program, you would first need to compile it using a COOL compiler. Once the program is compiled, you can run it by executing the following command:

```
./a.out
```

This will run the program and print the following messages to the console:

```
Hello from class A!
Hello from class B!
```