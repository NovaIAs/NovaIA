```visual basic
' This program calculates the factorial of a number using recursive functions.

' Define the recursive function to calculate the factorial.
Public Function Factorial(number As Integer) As Long
    If number = 0 Then
        Return 1 ' Base case: factorial of 0 is 1.
    Else
        Return number * Factorial(number - 1) ' Recursive case: multiply number by factorial of (number-1).
    End If
End Function

' Get the input number from the user.
Console.Write("Enter a non-negative integer: ")
Dim inputNumber As Integer = Integer.Parse(Console.ReadLine())

' Calculate the factorial of the input number.
Dim factorialResult As Long = Factorial(inputNumber)

' Display the result.
Console.WriteLine("The factorial of {0} is {1}", inputNumber, factorialResult)

' Keep the console window open until the user presses any key.
Console.ReadKey()
```

Explanation:
This program calculates the factorial of a non-negative integer using recursive functions. Here's how it works:

1. Define the Recursive Function (Factorial):
   - We define a public function named Factorial that takes an integer number as input and returns a Long value (representing the factorial).
   - The function uses a recursive approach to calculate the factorial.

2. Base Case:
   - If the number is equal to 0, the function returns 1. This is the base case because the factorial of 0 is defined as 1.

3. Recursive Case:
   - For numbers greater than 0, the function calculates the factorial by multiplying the number with the factorial of (number-1).
   - This step is repeated until the number becomes 0, at which point the recursion stops and the calculated factorial is returned.

4. Getting User Input:
   - The program prompts the user to enter a non-negative integer using Console.Write.
   - The user's input is read using Console.ReadLine and converted to an integer using Integer.Parse.

5. Calculating Factorial:
   - The program calls the Factorial function with the input number as an argument to calculate the factorial.
   - The result of the factorial calculation is stored in the factorialResult variable.

6. Displaying the Result:
   - The program uses Console.WriteLine to display the calculated factorial along with the input number.

7. Keeping the Console Open:
   - Console.ReadKey is used to keep the console window open until the user presses any key. This prevents the program from closing immediately after displaying the result.