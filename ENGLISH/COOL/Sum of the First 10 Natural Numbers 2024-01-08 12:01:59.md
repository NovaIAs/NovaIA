```cool
class Main {
  main() : Object => {
    Int sum := 0;
    for (i in 1 to 10) {
      sum := sum + i;
    };
    sum.print();
  };
};
```

Explanation:

1. The `Main` class is defined, which contains the `main` method.
2. The `main` method is defined as a function that returns an `Object` and takes no arguments.
3. An integer variable named `sum` is declared and initialized to 0.
4. A `for` loop is used to iterate through the numbers from 1 to 10.
5. Inside the loop, the value of `sum` is incremented by the current value of `i`.
6. After the loop, the value of `sum` is printed to the console using the `print` method.