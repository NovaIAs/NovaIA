```visual basic
Public Class ComplexCodeExample

    ' Define the Fibonacci series function
    Private Function Fibonacci(n As Integer) As Integer
        If n < 2 Then
            Return n
        Else
            Return Fibonacci(n - 1) + Fibonacci(n - 2)
        End If
    End Function

    ' Define the main method
    Public Shared Sub Main(args As String())
        Dim n As Integer = 10 ' The number of Fibonacci numbers to generate
        Dim result As String = "" ' The string to store the Fibonacci numbers

        For i As Integer = 0 To n - 1
            result &= Fibonacci(i).ToString() & ", " ' Add the Fibonacci number to the string
        Next

        ' Remove the last comma and space from the string
        result = result.Substring(0, result.Length - 2)

        ' Print the Fibonacci numbers to the console
        Console.WriteLine("The first {0} Fibonacci numbers are: {1}", n, result)
    End Sub

End Class
```

Explanation:

1. Define the Fibonacci Series Function (`Fibonacci`):
   - This function takes an integer `n` as input and calculates the nth Fibonacci number using a recursive approach.
   - If `n` is less than 2, it simply returns `n` (base case).
   - Otherwise, it recursively calls itself with `n-1` and `n-2` and returns the sum of those results (recursive case).

2. Define the Main Method (`Main`):
   - Set `n` to 10, which represents the number of Fibonacci numbers to generate.
   - Initialize an empty string `result` to store the Fibonacci numbers.

3. Generate the Fibonacci Numbers:
   - Use a `For` loop to iterate from 0 to `n-1`.
   - For each iteration, call the `Fibonacci` function with the current loop index `i` and append the result to the `result` string.

4. Remove the Last Comma and Space:
   - After the loop, the `result` string will have an extra comma and space at the end.
   - Use the `Substring` method to remove the last two characters from the string.

5. Print the Fibonacci Numbers:
   - Use the `Console.WriteLine` method to display the first `n` Fibonacci numbers to the console.