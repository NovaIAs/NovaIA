```cool
class Main {
  static main() -> Object {
    if (self.divisible(10, 2)) then
      IO.print_int(10)
    else
      IO.print_int(20)
    fi;
    return 0
  }

  divisible(x: Int, y: Int) -> Bool {
    return x % y == 0
  }
}
```

This code checks if the number 10 is divisible by 2. If it is, it prints 10, otherwise it prints 20.

The code uses the `divisible` function, which takes two arguments, `x` and `y`, and returns a boolean value indicating whether `x` is divisible by `y`. The `divisible` function uses the modulo operator `%` to determine if `x` is divisible by `y`.

The code also uses the `if` statement to check the result of the `divisible` function and print the appropriate value. The `if` statement uses the `then` and `else` keywords to specify the actions to be taken if the condition is true or false, respectively.

The code also uses the `return` statement to return the value 0 from the `main` function. This value indicates to the operating system that the program has executed successfully.