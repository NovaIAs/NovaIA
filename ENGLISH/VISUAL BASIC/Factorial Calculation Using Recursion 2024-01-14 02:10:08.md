```visual basic
' This program calculates the factorial of a number using recursion.

' Declare the function that will calculate the factorial.
Function Factorial(number As Integer) As Integer
    ' If the number is 0, return 1.
    If number = 0 Then
        Return 1
    ' Otherwise, return the number multiplied by the factorial of the number minus 1.
    Else
        Return number * Factorial(number - 1)
    End If
End Function

' Get the number from the user.
Console.WriteLine("Enter a number:")
Dim number As Integer = Console.ReadLine()

' Calculate the factorial of the number.
Dim factorial As Integer = Factorial(number)

' Display the result.
Console.WriteLine("The factorial of {0} is {1}", number, factorial)
```

Explanation:

1. The program starts by declaring the `Factorial` function, which takes an integer `number` as an argument and returns an integer as the result.
2. Inside the `Factorial` function, there is a conditional statement that checks if the `number` is equal to 0. If it is, the function returns 1. This is because the factorial of 0 is defined as 1.
3. If the `number` is not equal to 0, the function returns the `number` multiplied by the factorial of the `number` minus 1. This is the recursive part of the function, where it calls itself with a smaller value of `number`.
4. After the `Factorial` function is defined, the program gets the number from the user using the `Console.ReadLine()` method.
5. The program then calls the `Factorial` function with the user-entered number and stores the result in the `factorial` variable.
6. Finally, the program displays the result using the `Console.WriteLine()` method.

This program demonstrates recursion, which is a technique where a function calls itself to solve a problem. In this case, the `Factorial` function calls itself to calculate the factorial of a number.