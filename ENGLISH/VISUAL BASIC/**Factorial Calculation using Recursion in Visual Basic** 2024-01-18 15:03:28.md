```visual basic
' Declare a function for calculating the factorial of a number.
Public Function Factorial(n As Integer) As Integer
    If n < 0 Then
        Throw New ArgumentOutOfRangeException("n", "n must be a non-negative integer.")
    ElseIf n = 0 Then
        Return 1
    Else
        Return n * Factorial(n - 1)
    End If
End Function

Private Sub Main()
    ' Create an array of numbers.
    Dim numbers As Integer() = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

    ' Calculate the factorial of each number in the array.
    Dim factorials As Integer() = For Each n In numbers
                                         Factorial(n)
                                     Next

    ' Display the results.
    For i As Integer = 0 To factorials.Length - 1
        Console.WriteLine("The factorial of {0} is {1}", numbers(i), factorials(i))
    Next

    Console.ReadKey()
End Sub
```

This code defines a function called `Factorial` that calculates the factorial of a number. The factorial of a number is the product of all the positive integers less than or equal to that number. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

The `Main` subroutine creates an array of numbers and then uses the `Factorial` function to calculate the factorial of each number in the array. The results are then displayed to the console.

Here is a more detailed explanation of the code:

* The `Factorial` function is a recursive function. This means that it calls itself to solve the problem. In this case, the function calls itself to calculate the factorial of the number that is one less than the number that was originally passed to the function. This process continues until the number reaches 0, at which point the function returns 1.
* The `For Each` loop in the `Main` subroutine iterates through the array of numbers and calls the `Factorial` function for each number in the array. The results of the function calls are stored in the `factorials` array.
* The `Console.WriteLine` statements in the `Main` subroutine display the results of the function calls to the console.

This code is complex because it uses recursion to solve a problem. Recursion is a powerful technique that can be used to solve a wide variety of problems, but it can also be difficult to understand. If you are having trouble understanding the code, I recommend that you read some tutorials on recursion before trying to use it in your own code.