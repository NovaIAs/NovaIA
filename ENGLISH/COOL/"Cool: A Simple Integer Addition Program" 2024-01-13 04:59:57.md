```cool
class Main inherits IO {
  main(): Self {
    let n: Int <- 10;
    let m: Int <- 20;
    let result: Int <- self.sum(n, m);
    self.out_int(result);
    return self;
  };

  sum(n: Int, m: Int): Int {
    let sum: Int <- n + m;
    return sum;
  };
};
```

Explanation:

1. `class Main inherits IO { ... }`: This line defines a new class called `Main` that inherits from the `IO` class. The `IO` class provides basic input and output functionality.

2. `main(): Self { ... }`: This is the main method of the program. It takes no arguments and returns an object of type `Self`, which refers to the current instance of the `Main` class.

3. `let n: Int <- 10;`: This line declares and initializes an integer variable `n` with the value 10.

4. `let m: Int <- 20;`: This line declares and initializes an integer variable `m` with the value 20.

5. `let result: Int <- self.sum(n, m);`: This line calls the `sum` method on the current instance of the `Main` class, passing in the values of `n` and `m` as arguments. The result of the method call is stored in the variable `result`.

6. `self.out_int(result);`: This line calls the `out_int` method on the current instance of the `Main` class, passing in the value of `result` as an argument. This prints the value of `result` to the console.

7. `return self;`: This line returns the current instance of the `Main` class. This is necessary because the main method is declared to return an object of type `Self`.

8. `sum(n: Int, m: Int): Int { ... }`: This is a method of the `Main` class that takes two integer arguments, `n` and `m`, and returns an integer result.

9. `let sum: Int <- n + m;`: This line calculates the sum of `n` and `m` and stores the result in the variable `sum`.

10. `return sum;`: This line returns the value of `sum` as the result of the method.