```csharp
// This code creates a simple calculator that can perform basic arithmetic operations.

// Create a class called Calculator to encapsulate the calculator's functionality.
public class Calculator
{
    // Define the available operations.
    public enum Operation
    {
        Addition,
        Subtraction,
        Multiplication,
        Division
    }

    // Define a method to perform the calculation.
    public double Calculate(double operand1, double operand2, Operation operation)
    {
        double result = 0;

        switch (operation)
        {
            case Operation.Addition:
                result = operand1 + operand2;
                break;
            case Operation.Subtraction:
                result = operand1 - operand2;
                break;
            case Operation.Multiplication:
                result = operand1 * operand2;
                break;
            case Operation.Division:
                // Check for division by zero.
                if (operand2 == 0)
                {
                    throw new DivideByZeroException("Cannot divide by zero.");
                }
                result = operand1 / operand2;
                break;
        }

        return result;
    }
}

// Create an instance of the Calculator class.
Calculator calculator = new Calculator();

// Get the operands and the operation from the user.
Console.Write("Enter the first operand: ");
double operand1 = double.Parse(Console.ReadLine());

Console.Write("Enter the second operand: ");
double operand2 = double.Parse(Console.ReadLine());

Console.Write("Enter the operation (+, -, *, /): ");
string operation = Console.ReadLine();

// Convert the operation to the corresponding enum value.
Operation operationEnum = (Operation)Enum.Parse(typeof(Operation), operation);

// Perform the calculation.
double result = calculator.Calculate(operand1, operand2, operationEnum);

// Display the result.
Console.WriteLine("The result is: {0}", result);
```

This code creates a simple calculator that can perform basic arithmetic operations. The code first defines a class called Calculator to encapsulate the calculator's functionality. The Calculator class has a method called Calculate that takes two operands and an operation as input and returns the result of the calculation.

The code then creates an instance of the Calculator class and gets the operands and the operation from the user. The code then converts the operation to the corresponding enum value and calls the Calculate method to perform the calculation. Finally, the code displays the result of the calculation.