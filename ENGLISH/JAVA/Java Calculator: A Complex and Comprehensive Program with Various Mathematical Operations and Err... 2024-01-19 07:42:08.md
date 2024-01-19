```java
import java.util.Scanner;

public class ComplexJavaCode {

    private static final Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        // Initialize variables
        int choice;
        double num1, num2, result;
        String operator;

        // Display menu
        System.out.println("Calculator Menu");
        System.out.println("1. Addition");
        System.out.println("2. Subtraction");
        System.out.println("3. Multiplication");
        System.out.println("4. Division");
        System.out.println("5. Power");
        System.out.println("6. Square Root");
        System.out.println("7. Exit");

        // Get user choice
        System.out.print("Enter your choice: ");
        choice = scanner.nextInt();

        // Perform calculation based on user choice
        switch (choice) {
            case 1:
                // Addition
                System.out.print("Enter first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter second number: ");
                num2 = scanner.nextDouble();
                result = num1 + num2;
                System.out.println("Result: " + result);
                break;
            case 2:
                // Subtraction
                System.out.print("Enter first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter second number: ");
                num2 = scanner.nextDouble();
                result = num1 - num2;
                System.out.println("Result: " + result);
                break;
            case 3:
                // Multiplication
                System.out.print("Enter first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter second number: ");
                num2 = scanner.nextDouble();
                result = num1 * num2;
                System.out.println("Result: " + result);
                break;
            case 4:
                // Division
                System.out.print("Enter first number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter second number: ");
                num2 = scanner.nextDouble();

                // Check for division by zero
                if (num2 == 0) {
                    System.out.println("Error: Division by zero is undefined");
                } else {
                    result = num1 / num2;
                    System.out.println("Result: " + result);
                }
                break;
            case 5:
                // Power
                System.out.print("Enter base number: ");
                num1 = scanner.nextDouble();
                System.out.print("Enter exponent: ");
                num2 = scanner.nextDouble();
                result = Math.pow(num1, num2);
                System.out.println("Result: " + result);
                break;
            case 6:
                // Square Root
                System.out.print("Enter a number: ");
                num1 = scanner.nextDouble();

                // Check for negative numbers
                if (num1 < 0) {
                    System.out.println("Error: Square root of negative numbers is undefined");
                } else {
                    result = Math.sqrt(num1);
                    System.out.println("Result: " + result);
                }
                break;
            case 7:
                // Exit the program
                System.out.println("Exiting the program...");
                System.exit(0);
                break;
            default:
                // Invalid choice
                System.out.println("Invalid choice. Please enter a valid number between 1 and 7.");
        }
    }
}
```

This Java code is a complex and differentiated program that acts as a simple calculator with various mathematical operations and error handling. It presents a menu-driven interface for the user to choose from different operations, including addition, subtraction, multiplication, division, power, square root, and exit. Let's explain the code step by step:

1. **Program Initialization**:
   - The program starts by initializing necessary variables, including the user's choice, numbers for calculations, and result.

2. **Menu Display**:
   - The program displays a menu of options for the user to select the desired operation.

3. **User Input**:
   - The user is prompted to enter their choice for the operation they want to perform.

4. **Switch-Case Statement**:
   - A switch-case statement is used to handle different operations based on the user's choice.

5. **Mathematical Operations**:
   - For each operation (addition, subtraction, multiplication, division, power, and square root), the program prompts the user to enter necessary numbers and performs the calculation.

6. **Error Handling**:
   - The program checks for potential errors, such as division by zero or negative numbers for square root, and displays error messages if necessary.

7. **Result Output**:
   - After performing the calculation, the program displays the result to the user.

8. **Exit Option**:
   - The user can choose to exit the program by selecting option 7 from the menu.

9. **Error Handling for Invalid Input**:
   - The program handles invalid user input by displaying an error message if the user enters a choice outside the valid range (1-7).

This code showcases various concepts in Java, including user input, mathematical operations, conditional statements, and error handling, making it a complex and comprehensive program to demonstrate programming skills.