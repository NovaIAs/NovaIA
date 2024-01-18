```csharp
// This C# code demonstrates how to create a complex and differentiated code with various features.

// 1. Define a delegate for a mathematical operation.
delegate double MathOperation(double a, double b);

// 2. Define a class called 'Calculator' with various mathematical operations.
class Calculator
{
    // Addition operation delegate.
    public static MathOperation Add = (a, b) => a + b;

    // Subtraction operation delegate.
    public static MathOperation Subtract = (a, b) => a - b;

    // Multiplication operation delegate.
    public static MathOperation Multiply = (a, b) => a * b;

    // Division operation delegate.
    public static MathOperation Divide = (a, b) => a / b;

    // Power operation delegate.
    public static MathOperation Power = (a, b) => Math.Pow(a, b);

    // Square root operation delegate.
    public static MathOperation SquareRoot = (a, b) => Math.Sqrt(a);

    // Calculate the result based on the specified operation.
    public double Calculate(double a, double b, MathOperation operation)
    {
        return operation(a, b);
    }
}

// 3. Define a method to print a customized border.
static void PrintBorder(int size, char borderChar)
{
    Console.WriteLine(new string(borderChar, size));
}

// 4. Define a method to align text within a specified width.
static string AlignText(string text, int width, char paddingChar = ' ')
{
    int paddingLength = width - text.Length;
    return paddingLength > 0 ? text.PadRight(width, paddingChar) : text;
}

// 5. Define a method to create a menu of options.
static int DisplayMenu(string[] options)
{
    int selectedOption = 0;
    ConsoleKeyInfo keyInfo;

    do
    {
        Console.Clear();

        // Print the menu options.
        foreach (var option in options)
        {
            Console.WriteLine(AlignText(option, 30, '-'));
        }

        // Get the user's input.
        keyInfo = Console.ReadKey(true);

        // Handle arrow key presses.
        if (keyInfo.Key == ConsoleKey.UpArrow)
        {
            selectedOption--;
            if (selectedOption < 0)
            {
                selectedOption = options.Length - 1;
            }
        }
        else if (keyInfo.Key == ConsoleKey.DownArrow)
        {
            selectedOption++;
            if (selectedOption >= options.Length)
            {
                selectedOption = 0;
            }
        }

        // Handle enter key press.
        if (keyInfo.Key == ConsoleKey.Enter)
        {
            break;
        }
    } while (true);

    return selectedOption;
}

// 6. Define a method to handle user input and perform calculations.
static void HandleUserInput()
{
    // Create a calculator object.
    Calculator calculator = new Calculator();

    // Available mathematical operations.
    string[] operations = { "Addition", "Subtraction", "Multiplication", "Division", "Power", "Square Root" };

    // Display the operations menu.
    int selectedOperation = DisplayMenu(operations);

    // Get the user's input for the first number.
    Console.Write("\nEnter the first number: ");
    double num1 = double.Parse(Console.ReadLine());

    // Get the user's input for the second number.
    Console.Write("Enter the second number: ");
    double num2 = double.Parse(Console.ReadLine());

    // Perform the calculation based on the selected operation.
    double result = calculator.Calculate(num1, num2, operations[selectedOperation] == "Power" ? Calculator.Power : Calculator.SquareRoot);

    // Print the result.
    Console.WriteLine($"\nResult: {result}");
}

// 7. Main program entry point.
class Program
{
    static void Main(string[] args)
    {
        // Print a welcome message.
        PrintBorder(50, '-');
        Console.WriteLine(AlignText("Welcome to the Complex Calculator!", 50));
        PrintBorder(50, '-');

        // Handle user input and perform calculations.
        HandleUserInput();

        // Exit the program.
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}
```

Explanation:

1. We define a delegate called 'MathOperation' to represent a mathematical operation that takes two double parameters and returns a double result.

2. We define a class called 'Calculator' that contains various mathematical operations as static methods, represented by delegate instances.

3. We define a method called 'PrintBorder' to print a border of a specified size using a specified character.

4. We define a method called 'AlignText' to align text within a specified width, with optional padding.

5. We define a method called 'DisplayMenu' to display a menu of options and handle user input using arrow keys and the enter key.

6. We define a method called 'HandleUserInput' to get user input, perform calculations based on the selected operation, and display the result.

7. In the 'Main' method, we print a welcome message, handle user input and perform calculations, and exit the program when the user presses any key.