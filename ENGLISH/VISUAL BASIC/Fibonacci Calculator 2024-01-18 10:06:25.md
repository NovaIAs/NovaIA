```visual basic
Public Function Fibonacci(n As Integer) As Long
'Calculates the nth Fibonacci number using a recursive function.

    If n <= 1 Then
        Fibonacci = n
    Else
        Fibonacci = Fibonacci(n - 1) + Fibonacci(n - 2)
    End If

End Function

Public Sub Main()
'Demonstrates the Fibonacci function by calculating and displaying the first 20 Fibonacci numbers.

    Dim i As Integer
    Dim fibonacciNumber As Long

    For i = 0 To 19
        fibonacciNumber = Fibonacci(i)
        Console.WriteLine("The {0}th Fibonacci number is {1}", i, fibonacciNumber)
    Next i

End Sub
```
Explanation:

1. `Public Function Fibonacci(n As Integer) As Long`: This line declares a public function named `Fibonacci` that takes an integer parameter `n` and returns a long integer result. The function calculates the nth Fibonacci number.

2. `If n <= 1 Then`: This line checks if `n` is less than or equal to 1. If it is, the function returns `n` as the result. This is the base case for the recursion, as the Fibonacci sequence starts with 0 and 1.

3. `Else`: If `n` is greater than 1, the function enters the `Else` block.

4. `Fibonacci = Fibonacci(n - 1) + Fibonacci(n - 2)`: This line calculates the nth Fibonacci number by recursively calling the `Fibonacci` function with `n - 1` and `n - 2` as arguments. The result of these recursive calls are added together to get the nth Fibonacci number.

5. `End If`: This line ends the `If...Else` statement.

6. `Public Sub Main()`: This line declares a public sub procedure named `Main`, which is the entry point of the program.

7. `Dim i As Integer`: This line declares an integer variable named `i`.

8. `Dim fibonacciNumber As Long`: This line declares a long integer variable named `fibonacciNumber`.

9. `For i = 0 To 19`: This line starts a `For` loop that iterates from 0 to 19. This loop will calculate and display the first 20 Fibonacci numbers.

10. `fibonacciNumber = Fibonacci(i)`: This line calls the `Fibonacci` function with the current value of `i` as the argument and assigns the result to the `fibonacciNumber` variable.

11. `Console.WriteLine("The {0}th Fibonacci number is {1}", i, fibonacciNumber)`: This line uses the `Console.WriteLine` method to display the current Fibonacci number along with its index `i`. The `{0}` and `{1}` placeholders are used to format the output string.

12. `Next i`: This line increments the `i` variable and continues the `For` loop until `i` reaches 19.

13. `End Sub`: This line ends the `Main` sub procedure.

When you run this program, it will calculate and display the first 20 Fibonacci numbers. The output will look something like this:

```
The 0th Fibonacci number is 0
The 1st Fibonacci number is 1
The 2nd Fibonacci number is 1
The 3rd Fibonacci number is 2
The 4th Fibonacci number is 3
The 5th Fibonacci number is 5
The 6th Fibonacci number is 8
The 7th Fibonacci number is 13
The 8th Fibonacci number is 21
The 9th Fibonacci number is 34
The 10th Fibonacci number is 55
The 11th Fibonacci number is 89
The 12th Fibonacci number is 144
The 13th Fibonacci number is 233
The 14th Fibonacci number is 377
The 15th Fibonacci number is 610
The 16th Fibonacci number is 987
The 17th Fibonacci number is 1597
The 18th Fibonacci number is 2584
The 19th Fibonacci number is 4181
```