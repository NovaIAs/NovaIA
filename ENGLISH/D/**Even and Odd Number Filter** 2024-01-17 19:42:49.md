```d
import std.algorithm, std.stdio, std.string, std.range;

void main() {
    auto numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto evens = numbers.filter!(x => x % 2 == 0);

    auto odds = numbers.filter!(x => x % 2 == 1);

    writefln("Even numbers:");
    evens.foreach!(x => writefln("\t%d", x));

    writefln("Odd numbers:");
    odds.foreach!(x => writefln("\t%d", x));
}
```

Explanation:

1. We import the necessary libraries:

   - `std.algorithm`: Contains various algorithms and data structures.
   - `std.stdio`: Provides functions for input and output operations.
   - `std.string`: Contains functions for working with strings.
   - `std.range`: Provides functions for working with ranges (sequences of elements).

2. We define a list of numbers using the `[]` syntax:

   ```
   auto numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
   ```

3. We use the `filter!` function from the `std.algorithm` library to filter the even and odd numbers from the `numbers` list:

   ```
   auto evens = numbers.filter!(x => x % 2 == 0);
   ```

   - `filter!` takes a lambda expression (`x => x % 2 == 0`) as an argument, which checks if a number is even.
   - It returns a new list containing only the even numbers.

4. Similarly, we filter the odd numbers:

   ```
   auto odds = numbers.filter!(x => x % 2 == 1);
   ```

5. We use `writefln` from the `std.stdio` library to print the even and odd numbers:

   ```
   writefln("Even numbers:");
   evens.foreach!(x => writefln("\t%d", x));

   writefln("Odd numbers:");
   odds.foreach!(x => writefln("\t%d", x));
   ```

   - `foreach!` iterates over the `evens` and `odds` lists and prints each element.