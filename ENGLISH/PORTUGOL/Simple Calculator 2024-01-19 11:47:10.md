```portugol
// This program simulates a simple calculator.
// It can perform basic arithmetic operations (addition, subtraction, multiplication, and division) on two numbers.

var num1, num2: real;
var op: char;

// Get the numbers and the operation from the user.
write("Enter the first number: ");
readln(num1);
write("Enter the second number: ");
readln(num2);
write("Enter the operation (+, -, *, /): ");
readln(op);

// Perform the operation based on the user's input.
if op = "+" then
    write("The result is: ", num1 + num2);
elif op = "-" then
    write("The result is: ", num1 - num2);
elif op = "*" then
    write("The result is: ", num1 * num2);
elif op = "/" then
    if num2 = 0 then
        write("Division by zero is not allowed.");
    else
        write("The result is: ", num1 / num2);
    end if;
else
    write("Invalid operation.");
end if;

```

 **Explanation:**

1. `var num1, num2: real;`: Declare two variables `num1` and `num2` of type `real` (floating-point numbers) to store the two numbers entered by the user.

2. `var op: char;`: Declare a variable `op` of type `char` to store the operation (+, -, *, /) entered by the user.

3. `write("Enter the first number: ");` and `readln(num1);`: Prompt the user to enter the first number and read it into the variable `num1`.

4. `write("Enter the second number: ");` and `readln(num2);`: Prompt the user to enter the second number and read it into the variable `num2`.

5. `write("Enter the operation (+, -, *, /): ");` and `readln(op);`: Prompt the user to enter the operation (+, -, *, /) and read it into the variable `op`.

6. `if op = "+" then`: Check if the user entered the addition operation (+).

7. `write("The result is: ", num1 + num2);`: If the user entered the addition operation, perform the addition of `num1` and `num2` and display the result.

8. `elif op = "-" then`: Check if the user entered the subtraction operation (-).

9. `write("The result is: ", num1 - num2);`: If the user entered the subtraction operation, perform the subtraction of `num2` from `num1` and display the result.

10. `elif op = "*" then`: Check if the user entered the multiplication operation (*).

11. `write("The result is: ", num1 * num2);`: If the user entered the multiplication operation, perform the multiplication of `num1` and `num2` and display the result.

12. `elif op = "/" then`: Check if the user entered the division operation (/).

13. `if num2 = 0 then`: Check if the second number (denominator) is zero.

14. `write("Division by zero is not allowed.");`: If the second number is zero, display an error message because division by zero is not allowed.

15. `else`: If the second number is not zero, perform the division of `num1` by `num2` and display the result.

16. `end if;`: End the `if` statement that checks for the division operation.

17. `else`: If the user entered an invalid operation (not +, -, *, or /), display an error message.

18. `end if;`: End the `if` statement that checks for the operation entered by the user.

This code covers all the basic operations of a simple calculator, including handling the case of division by zero. It provides a comprehensive solution to the task of simulating a calculator in Portugol.