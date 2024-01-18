```cool
% This is a COOL program that computes the Fibonacci numbers.
% Fibonacci numbers are defined as follows:
% F(0) = 0
% F(1) = 1
% F(n) = F(n-1) + F(n-2) for n > 1

class Fibonacci {
  % This is the main method. It is called when the program starts.
  main() {
    % Print the Fibonacci numbers up to 100.
    [0,1].foreach(print_fibonacci(100));
  }

  % This method prints the Fibonacci numbers up to a given limit.
  print_fibonacci(limit: Int) {
    % Initialize the first two Fibonacci numbers.
    var fib_0: Int <- 0;
    var fib_1: Int <- 1;

    % Print the first two Fibonacci numbers.
    print(fib_0, ", ");
    print(fib_1, ", ");

    % Compute and print the remaining Fibonacci numbers up to the limit.
    while (fib_1 < limit) {
      % Compute the next Fibonacci number.
      var next_fib: Int <- fib_0 + fib_1;

      % Print the next Fibonacci number.
      print(next_fib, ", ");

      % Update the first two Fibonacci numbers.
      fib_0 <- fib_1;
      fib_1 <- next_fib;
    }

    % Print a newline.
    print_newline();
  }
}

% Create an instance of the Fibonacci class and call its main method.
new Fibonacci().main();
```

Explanation:

* The Fibonacci class contains two methods: `main` and `print_fibonacci`.
* The `main` method is the main entry point of the program. It calls the `print_fibonacci` method to print the Fibonacci numbers up to 100.
* The `print_fibonacci` method takes a limit as an argument and prints the Fibonacci numbers up to that limit.
* The method first initializes the first two Fibonacci numbers to 0 and 1, respectively.
* It then prints the first two Fibonacci numbers.
* The method then enters a while loop that computes and prints the remaining Fibonacci numbers up to the limit.
* The method updates the first two Fibonacci numbers after each iteration of the while loop.
* Finally, the method prints a newline.
* The `new Fibonacci().main()` statement creates an instance of the Fibonacci class and calls its `main` method. This starts the program.