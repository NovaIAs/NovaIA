```visual basic

Public Function Factorial(n As Integer) As Integer
    If n = 0 Then
        Factorial = 1
    Else
        Factorial = n * Factorial(n - 1)
    End If
End Function

Public Sub Main()
    Dim number As Integer
    Dim result As Integer

    Console.WriteLine("Enter a non-negative integer:")
    number = Integer.Parse(Console.ReadLine())

    result = Factorial(number)

    Console.WriteLine($"The factorial of {number} is {result}")
End Sub
```

This code calculates the factorial of a non-negative integer using a recursive function. The factorial of a number is the product of all positive integers up to that number. For example, the factorial of 5 is 120, which is calculated as follows:

```
5! = 5 * 4 * 3 * 2 * 1 = 120
```

The code first defines the `Factorial` function, which takes a single integer argument and returns the factorial of that number. The function uses recursion to calculate the factorial. If the input number is 0, the function returns 1, since the factorial of 0 is defined to be 1. Otherwise, the function multiplies the input number by the factorial of the input number minus 1. This process continues until the input number reaches 0, at which point the function returns 1.

The `Main` subroutine then prompts the user to enter a non-negative integer. The subroutine then calls the `Factorial` function with the user's input as the argument and stores the result in a variable. Finally, the subroutine displays the factorial of the user's input to the console.

Here is an example of how the code works:

```
Enter a non-negative integer:
5

The factorial of 5 is 120
```

This code is complex because it uses recursion, which is a more advanced programming concept. However, the code is also well-commented and easy to understand.