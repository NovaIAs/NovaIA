```portugal
program fibonacciSeries;
{This program prints the Fibonacci series up to a specified number.}

const
  maxNumber = 100;
var
  a, b, c: integer;
begin
  a := 0;
  b := 1;
  while c < maxNumber do
  begin
    c := a + b;
    if c < maxNumber then
      write(c, " ");
    a := b;
    b := c;
  end;
  writeln;
end.
```

This program uses a while loop to generate the Fibonacci series. The Fibonacci series is a series of numbers where each number is the sum of the two previous numbers. The series starts with 0 and 1, and continues with 1, 2, 3, 5, 8, 13, and so on.

The program starts by defining a constant called `maxNumber`. This constant specifies the maximum number that the program will print in the Fibonacci series.

The program then defines three variables: `a`, `b`, and `c`. The variables `a` and `b` are used to store the previous two numbers in the Fibonacci series, and the variable `c` is used to store the current number in the series.

The program then enters a while loop. The condition of the while loop is `c < maxNumber`. This condition checks to see if the current number in the series is less than the maximum number. If the condition is true, the program continues to execute the loop. Otherwise, the program exits the loop.

Inside the loop, the program calculates the next number in the Fibonacci series. The next number is calculated by adding the previous two numbers in the series. The program then stores the next number in the variable `c`.

The program then checks to see if the next number is less than the maximum number. If the next number is less than the maximum number, the program prints the next number to the console.

The program then updates the variables `a` and `b`. The variable `a` is set to the value of the variable `b`, and the variable `b` is set to the value of the variable `c`.

The program then repeats the loop, and the process continues until the current number in the series is greater than or equal to the maximum number.

Once the loop has finished executing, the program prints a newline character to the console. This newline character separates the Fibonacci series from the rest of the output.