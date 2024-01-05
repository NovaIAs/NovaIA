```cool
-- This program implements the classic "Hello, world!" program in COOL.

class Main inherits Object {
  main() : Object {
    -- Print "Hello, world!" to the console.
    IO.out_string("Hello, world!\n");

    -- Return the special value `null` to indicate that the program has
    -- completed successfully.
    return null;
  }
}
```

Explanation:

* The `class Main inherits Object` line defines a new class called `Main` that inherits from the built-in `Object` class. This means that `Main` has access to all of the methods and properties of `Object`.
* The `main() : Object` line defines the `main` method of the `Main` class. This is the entry point for the program.
* The `IO.out_string("Hello, world!\n");` line uses the `IO.out_string` method to print "Hello, world!" to the console. The `\n` at the end of the string is a newline character, which causes the cursor to move to the next line after printing.
* The `return null;` line returns the special value `null` to indicate that the program has completed successfully.

When this program is run, it will print "Hello, world!" to the console and then exit.