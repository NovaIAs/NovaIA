```portugal
program factorial:

const
        max_n = 1000;
var
        n: integer;
        fatorial: [1..max_n] of integer;

procedure calculate_factorial(n: integer);
var
        i: integer;
begin
        fatorial[0] := 1;
        for i := 1 to n do
                fatorial[i] := i * fatorial[i - 1];
end;

begin
        write('Enter a non-negative integer: ');
        read(n);
        calculate_factorial(n);
        writeln('The factorial of ', n, ' is ', fatorial[n]);
end.
```

The code above calculates the factorial of a non-negative integer. The factorial of a non-negative integer n is the product of all positive integers less than or equal to n. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

The code first declares a constant named `max_n` with the value 1000. This constant specifies the maximum value of n that the program can handle.

Next, the code declares a variable named `n` of type integer. This variable will store the value of n entered by the user.

The code also declares an array named `fatorial` of type integer with a range of 0 to `max_n`. This array will store the factorial of each integer from 0 to `max_n`.

The code then defines a procedure named `calculate_factorial` that takes an integer parameter named `n`. This procedure calculates the factorial of n and stores it in the `fatorial` array.

The procedure starts by setting the factorial of 0 to 1. This is because the factorial of 0 is defined to be 1.

Next, the procedure loops through the values of n from 1 to n. For each value of n, the procedure calculates the factorial by multiplying the current value of n by the factorial of the previous value of n.

For example, to calculate the factorial of 5, the procedure would calculate the factorial of 4 by multiplying 4 by the factorial of 3. Then, the procedure would calculate the factorial of 5 by multiplying 5 by the factorial of 4.

After the procedure has calculated the factorial of all the values of n from 0 to n, it returns.

The main program then prompts the user to enter a non-negative integer. The program then calls the `calculate_factorial` procedure to calculate the factorial of the entered value.

Finally, the program prints the result of the factorial calculation to the console.