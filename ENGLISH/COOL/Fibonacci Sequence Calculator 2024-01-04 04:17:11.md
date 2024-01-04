```cool
-- This is a simple COOL program that prints the Fibonacci sequence.

class Fibonacci {
  -- The main method is the entry point for the program.
  main() : Object {
    -- Print the first 10 Fibonacci numbers.
    for i in 0 to 9 do
      print(fibonacci(i));
    end;
    return 0;
  };

  -- The fibonacci method calculates the nth Fibonacci number.
  fibonacci(n : Integer) : Integer {
    if n <= 1 then
      return n;
    else
      return fibonacci(n - 1) + fibonacci(n - 2);
    end;
  };
};
```

**Explanation:**

* The `Fibonacci` class defines the `main` method, which is the entry point for the program.
* The `main` method prints the first 10 Fibonacci numbers.
* The `fibonacci` method calculates the nth Fibonacci number.
* The Fibonacci sequence is defined by the following recurrence relation:

```
F(n) = F(n-1) + F(n-2)
```

* The base cases for the recurrence relation are:

```
F(0) = 0
F(1) = 1
```

* The `fibonacci` method uses a recursive algorithm to calculate the nth Fibonacci number.
* The time complexity of the `fibonacci` method is exponential, which means that it takes a long time to calculate large Fibonacci numbers.