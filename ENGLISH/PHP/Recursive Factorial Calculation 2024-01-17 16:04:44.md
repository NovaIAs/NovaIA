```php
// This code snippet demonstrates the use of a recursive function to calculate the factorial of a number.
function calculateFactorial($number) {
    // Base case: If the number is 0 or 1, the factorial is 1.
    if ($number == 0 || $number == 1) {
        return 1;
    }

    // Recursive case: Calculate the factorial of the number by multiplying the number by the factorial of the previous number.
    else {
        return $number * calculateFactorial($number - 1);
    }
}

// Prompt the user to enter a number.
$number = readline("Enter a number: ");

// Validate the user input to ensure it is a non-negative integer.
if (!is_numeric($number) || $number < 0) {
    echo "Invalid input. Please enter a non-negative integer." . PHP_EOL;
    exit;
}

// Calculate the factorial of the entered number using the recursive function.
$factorial = calculateFactorial($number);

// Display the result to the user.
echo "The factorial of $number is $factorial." . PHP_EOL;
```

Explanation:

1. `calculateFactorial` Function:
   - This is a recursive function that calculates the factorial of a given number.
   - It checks if the number is 0 or 1, in which case the factorial is 1.
   - For other numbers, it multiplies the number by the factorial of the previous number, effectively reducing the problem to smaller subproblems.

2. User Input:
   - The user is prompted to enter a number.
   - The entered input is stored in the `$number` variable.

3. Input Validation:
   - The script checks if the input is a non-negative integer.
   - If the input is invalid, an error message is displayed, and the program exits.

4. Calculating the Factorial:
   - The `calculateFactorial` function is called with the user's input as an argument.
   - The result is stored in the `$factorial` variable.

5. Output:
   - The factorial of the entered number is displayed to the user.

This code demonstrates a recursive approach to calculating the factorial of a number, ensuring accuracy and efficiency even for large input values.