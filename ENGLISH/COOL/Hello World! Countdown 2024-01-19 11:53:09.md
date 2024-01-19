```cool
class Main is
    main() is
        begin
            local i : Int := 1;
            local array a : Array[String] := new Array[String].from_string_list(["Hello", "World", "!"]);
            while i <= 3 do
                a[i].concat(" ").concat(String.int_to_string(i)).concat(", ").output();
                i := i + 1;
            end;
        end.
end Main.
```

This code is a program in the COOL programming language that prints the following to the console:

```
Hello World! 1,
Hello World! 2,
Hello World! 3,
```

The code first defines a class called `Main` that contains a single method called `main`. The `main` method is the entry point for the program.

Inside the `main` method, the code first declares a local variable called `i` of type `Int` and initializes it to 1. It then declares a local variable called `a` of type `Array[String]` and initializes it to a new array of strings containing the values "Hello", "World", and "!".

The code then enters a while loop that continues as long as the value of `i` is less than or equal to 3. Inside the loop, the code uses the `concat` method to concatenate the value of `a[i]` with a space, the value of `i` converted to a string, and a comma. The resulting string is then printed to the console using the `output` method.

Finally, the value of `i` is incremented by 1 and the loop continues. When the loop finishes, the program terminates.

Here is a breakdown of the code:

* The `class Main is` line declares a class called `Main`.
* The `main() is` line declares a method called `main` that takes no arguments and returns no value.
* The `begin` and `end` lines mark the beginning and end of the `main` method.
* The `local i : Int := 1;` line declares a local variable called `i` of type `Int` and initializes it to 1.
* The `local array a : Array[String] := new Array[String].from_string_list(["Hello", "World", "!"]);` line declares a local variable called `a` of type `Array[String]` and initializes it to a new array of strings containing the values "Hello", "World", and "!".
* The `while i <= 3 do` line starts a while loop that continues as long as the value of `i` is less than or equal to 3.
* The `a[i].concat(" ").concat(String.int_to_string(i)).concat(", ").output();` line uses the `concat` method to concatenate the value of `a[i]` with a space, the value of `i` converted to a string, and a comma. The resulting string is then printed to the console using the `output` method.
* The `i := i + 1;` line increments the value of `i` by 1.
* The `end;` line ends the while loop.