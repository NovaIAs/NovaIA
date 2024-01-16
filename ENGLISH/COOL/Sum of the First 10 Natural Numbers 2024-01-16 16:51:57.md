```cool
class Main {
  static void main() {
    Int n = 10;
    List<Int> numbers = new List<Int>(n);
    for (Int i = 0; i < n; i++) {
      numbers[i] = i;
    }
    Int sum = 0;
    for (Int i = 0; i < n; i++) {
      sum += numbers[i];
    }
    IO.print_int(sum);
  }
}
```

This code calculates the sum of the first 10 natural numbers using a loop.

* The `Main` class is the entry point of the program.
* The `main` method is the main function of the program.
* The `Int` type is a primitive type in COOL that represents an integer number.
* The `List<Int>` type is a generic type that represents a list of integers.
* The `new` keyword is used to create a new instance of a class or type.
* The `for` loop is used to iterate over a collection of elements.
* The `IO.print_int` method is used to print an integer number to the console.

The code first creates a list of 10 integer numbers and initializes each element of the list to its index.
Then, it creates a variable `sum` and initializes it to 0.
The code then uses a loop to iterate over the list and adds each element of the list to the variable `sum`.
Finally, the code prints the value of `sum` to the console using the `IO.print_int` method.