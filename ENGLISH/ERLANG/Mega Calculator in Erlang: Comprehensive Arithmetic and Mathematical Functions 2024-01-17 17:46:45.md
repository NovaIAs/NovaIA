```erlang
% START MODULE
-module(mega_calculator).

% DEFINE EXPORTED FUNCTIONS
-export([add/2, subtract/2, multiply/2, divide/2, factorial/1]).

% ADDITION FUNCTION
add(X, Y) -> X + Y.

% SUBTRACTION FUNCTION
subtract(X, Y) -> X - Y.

% MULTIPLICATION FUNCTION
multiply(X, Y) -> X * Y.

% DIVISION FUNCTION
divide(X, Y) when Y /= 0 -> X / Y;
divide(_, 0) -> "Error: Division by zero".

% FACTORIAL FUNCTION (USING RECURSION)
factorial(N) when N == 0 -> 1;
factorial(N) -> N * factorial(N-1).

% END MODULE
```

EXPLANATION:

1. START MODULE: This line begins the Erlang module named "mega_calculator." Modules are containers for functions in Erlang.


2. DEFINE EXPORTED FUNCTIONS: The -export/1 directive specifies which functions within this module should be exported and accessible from other modules. In this case, we export the functions add, subtract, multiply, divide, and factorial.


3. ADDITION FUNCTION: The add/2 function takes two arguments, X and Y, and returns their sum. The + operator in Erlang performs addition.


4. SUBTRACTION FUNCTION: The subtract/2 function is similar to the add function, but it subtracts Y from X and returns the result.


5. MULTIPLICATION FUNCTION: The multiply/2 function multiplies its two arguments, X and Y, using the * operator.


6. DIVISION FUNCTION: The divide/2 function checks if the second argument, Y, is not equal to zero. If Y is nonzero, it divides X by Y and returns the result. If Y is zero, it returns the error message "Error: Division by zero" to indicate an invalid division.


7. FACTORIAL FUNCTION: The factorial/1 function calculates the factorial of a given number. It uses recursion and a pattern-matching clause to handle the base case (0 factorial is 1) and the recursive case (N factorial is N times the factorial of N-1).

This code provides a comprehensive set of arithmetic and mathematical functions, including addition, subtraction, multiplication, division, and factorial calculation. It demonstrates the use of recursion and pattern-matching in Erlang, making it a more advanced code compared to simple examples.