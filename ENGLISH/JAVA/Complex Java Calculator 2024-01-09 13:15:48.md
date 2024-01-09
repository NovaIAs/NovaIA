```java
// Complex Java Code
import java.util.Scanner;

public class ComplexJavaCode {

    // Main Method
    public static void main(String[] args) {
        // Create a Scanner object for user input
        Scanner scanner = new Scanner(System.in);

        // Declare variables
        int choice;
        double num1, num2, result;

        // Display a menu of operations
        System.out.println("Choose an operation:");
        System.out.println("1. Addition");
        System.out.println("2. Subtraction");
        System.out.println("3. Multiplication");
        System.out.println("4. Division");
        System.out.println("5. Modulus");
        System.out.println("6. Power");
        System.out.println("7. Square Root");
        System.out.println("8. Exit");

        // Get the user's choice
        System.out.print("Enter your choice: ");
        choice = scanner.nextInt();

        // Perform the selected operation
        switch (choice) {
            case 1:
                // Addition
                System.out.print("Enter the first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the second number: ");
                num2 = scanner.nextDouble();
                result = num1 + num2;
                System.out.println("Result: " + result);
                break;
            case 2:
                // Subtraction
                System.out.print("Enter the first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the second number: ");
                num2 = scanner.nextDouble();
                result = num1 - num2;
                System.out.println("Result: " + result);
                break;
            case 3:
                // Multiplication
                System.out.print("Enter the first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the second number: ");
                num2 = scanner.nextDouble();
                result = num1 * num2;
                System.out.println("Result: " + result);
                break;
            case 4:
                // Division
                System.out.print("Enter the first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the second number: ");
                num2 = scanner.nextDouble();
                if (num2 == 0) {
                    System.out.println("Division by zero is undefined.");
                } else {
                    result = num1 / num2;
                    System.out.println("Result: " + result);
                }
                break;
            case 5:
                // Modulus
                System.out.print("Enter the first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the second number: ");
                num2 = scanner.nextDouble();
                result = num1 % num2;
                System.out.println("Result: " + result);
                break;
            case 6:
                // Power
                System.out.print("Enter the base number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter the exponent: ");
                num2 = scanner.nextDouble();
                result = Math.pow(num1, num2);
                System.out.println("Result: " + result);
                break;
            case 7:
                // Square Root
                System.out.print("Enter the number: ");
                num1 = scanner.nextDouble();
                if (num1 < 0) {
                    System.out.println("Square root of a negative number is undefined.");
                } else {
                    result = Math.sqrt(num1);
                    System.out.println("Result: " + result);
                }
                break;
            case 8:
                // Exit
                System.out.println("Exiting the program.");
                break;
            default:
                // Invalid choice
                System.out.println("Invalid choice. Please enter a number between 1 and 8.");
        }

        // Close the Scanner object
        scanner.close();
    }
}
```

**Explanation:**

This Java code is a complex program that performs various mathematical operations based on the user's choice. It includes addition, subtraction, multiplication, division, modulus, power, square root, and an exit option. The program uses a switch statement to handle the user's choice and perform the selected operation. It also includes error handling for division by zero and square root of negative numbers.

Here's a detailed explanation of the code:

1. **Import Statements**:
   ```java
   import java.util.Scanner;
   ```
   This line imports the `Scanner` class from the `java.util` package, which is used for reading user input.

2. **Main Method**:
   ```java
   public static void main(String[] args) {
       // ...
   }
   ```
   This is the entry point of the program. It defines the `main` method, which is called when the program starts.

3. **Variables**:
   ```java
   int choice;
   double num1, num2, result;
   ```
   These variables are used to store the user's choice, the two numbers involved in the operation, and the result of the operation.

4. **Menu of Operations**:
   ```java
   System.out.println("Choose an operation:");
   System.out.println("1. Addition");
   System.out.println("2. Subtraction");
   System.out.println("3. Multiplication");
   System.out.println("4. Division");
   System.out.println("5. Modulus");
   System.out.println("6. Power");
   System.out.println("7. Square Root");
   System.out.println("8. Exit");
   ```
   This section displays a menu of operations for the user to choose from.

5. **Get User's Choice**:
   ```java
   System.out.print("Enter your choice: ");
   choice = scanner.nextInt();
   ```
   This prompts the user to enter their choice of operation, and it stores the choice in the `choice` variable.

6. **Switch Statement**:
   ```java
   switch (choice) {
       // ...
   }
   ```
   The switch statement is used to handle the user's choice and perform the selected operation. Each case corresponds to a different operation.

7. **Operation Cases**:
   Inside the switch statement, there are cases for each operation: addition, subtraction, multiplication, division, modulus, power, square root, and exit. Each case performs the selected operation using the `num1` and `num2` variables and displays the result.

8. **Error Handling**:
   The code includes error handling for division by zero and square root of negative numbers. If the user enters invalid values, the program displays an error message.

9. **Close Scanner Object**:
   ```java
   scanner.close();
   ```
   After all operations are complete, the `Scanner` object is closed to release resources.

10. **Exit Option**:
    The program includes an exit option (case 8) that allows the user to terminate the program.

This code demonstrates a complex Java program that performs various mathematical operations based on the user's choice. It includes a menu of operations, error handling, and a user-friendly interface.