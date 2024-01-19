```cool
// This Cool program implements the famous "Hello world!" program.

class Main inherits IO {
  main() : Object {
    out_string("Hello world!");
    return 0;
  }
};
```

This code defines a class called `Main` that inherits from the `IO` class. The `main` method is the entry point of the program. It prints the string "Hello world!" to the standard output and returns 0 to indicate that the program exited successfully.

Here's a more complex and differentiated code in Cool that implements a simple calculator:

```cool
class Calculator inherits IO {
  // Declare methods for addition, subtraction, multiplication, and division
  add(x: Int, y: Int) : Int {
    return x + y;
  }

  subtract(x: Int, y: Int) : Int {
    return x - y;
  }

  multiply(x: Int, y: Int) : Int {
    return x * y;
  }

  divide(x: Int, y: Int) : Int {
    return x / y;
  }

  // Declare a method to handle user input and perform calculations
  calculate() : Void {
    // Prompt the user to enter two integers
    out_string("Enter two integers separated by a space: ");
    String input := in_string();

    // Split the input string into two integers
    Int[] numbers := split_string(input, " ");
    Int x := numbers[0];
    Int y := numbers[1];

    // Determine the operation to be performed based on user input
    String operation;
    out_string("Enter an operation (+, -, *, /): ");
    operation := in_string();

    // Perform the calculation based on the chosen operation
    Int result;
    if (operation == "+") {
      result := add(x, y);
    } else if (operation == "-") {
      result := subtract(x, y);
    } else if (operation == "*") {
      result := multiply(x, y);
    } else if (operation == "/") {
      result := divide(x, y);
    } else {
      out_string("Invalid operation. Please enter +, -, *, or /");
      return;
    }

    // Print the result of the calculation
    out_string("Result: ");
    out_int(result);
    out_ln();
  }

  // Declare a method to split a string based on a delimiter
  split_string(s: String, delimiter: String) : String[] {
    String[] result := new String[];
    Int start := 0;
    Int end;

    while (start < s.length()) {
      end := s.index_of(delimiter, start);
      if (end == -1) {
        end := s.length();
      }

      result.append(s.substring(start, end));
      start := end + delimiter.length();
    }

    return result;
  }

  main() : Object {
    // Keep prompting the user to perform calculations until they enter "quit"
    while (true) {
      out_string("Enter 'quit' to exit or press any key to continue: ");
      String input := in_string();

      if (input == "quit") {
        break;
      } else {
        calculate();
      }
    }

    return 0;
  }
};
```

This code defines a class called `Calculator` that inherits from the `IO` class. It has methods for addition, subtraction, multiplication, and division. It also has a `calculate` method that handles user input, determines the operation to be performed, and prints the result of the calculation. Additionally, it has a `split_string` method to split a string based on a delimiter. The `main` method repeatedly prompts the user to enter calculations until they enter "quit".

This code is more complex and differentiated than the "Hello world!" program because it involves multiple methods, user input handling, and conditional branching. It demonstrates various features of the Cool programming language, such as inheritance, method overloading, and string manipulation.