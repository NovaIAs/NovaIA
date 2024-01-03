Certainly! Here's a complex and unique code written in Visual Basic that demonstrates various concepts and functionality. Let's dive into it:

```vb
Imports System

Module MainModule
    Sub Main()
        Console.WriteLine("Welcome to the Complex Code!")

        ' Variable Declaration and Initialization
        Dim numberList As New List(Of Integer)()
        Dim random As New Random()
        Dim sum As Integer = 0

        ' Generate Random Numbers and Store them in the List
        For i As Integer = 1 To 10
            Dim randomNumber As Integer = random.Next(1, 100)
            numberList.Add(randomNumber)
        Next

        ' Display the Generated Numbers
        Console.WriteLine("Generated Numbers:")
        For Each number As Integer In numberList
            Console.WriteLine(number)
        Next

        ' Perform Calculations on the Numbers
        For Each number As Integer In numberList
            If number Mod 2 = 0 Then
                sum += number
            End If
        Next

        ' Display the Sum of Even Numbers
        Console.WriteLine("Sum of Even Numbers: " & sum)

        ' Reverse the List of Numbers
        numberList.Reverse()

        ' Display the Reversed List
        Console.WriteLine("Reversed List:")
        For Each number As Integer In numberList
            Console.WriteLine(number)
        Next

        ' Find the Maximum Number in the List
        Dim maxNumber As Integer = numberList.Max()

        ' Display the Maximum Number
        Console.WriteLine("Maximum Number: " & maxNumber)

        ' Wait for User Input to Exit
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

Explanation:
1. The code starts by importing the `System` namespace, which provides various fundamental types and classes.
2. The code defines a module named `MainModule`, which contains the `Main` subroutine. This is the entry point of the program.
3. Inside the `Main` subroutine, a welcome message is displayed to the console.
4. Two variables, `numberList` (a list of integers) and `random` (an instance of the `Random` class), are declared and initialized.
5. A loop is used to generate 10 random numbers between 1 and 100 using the `random.Next` method and store them in the `numberList`.
6. The generated numbers are displayed on the console.
7. Another loop is used to calculate the sum of even numbers in the `numberList`.
8. The sum of even numbers is displayed on the console.
9. The `Reverse` method is called on the `numberList` to reverse the order of the numbers.
10. The reversed list is displayed on the console.
11. The `Max` method is called on the `numberList` to find the maximum number.
12. The maximum number is displayed on the console.
13. A message is displayed to prompt the user to exit the program.
14. The program waits for the user to press any key before exiting.

This code demonstrates the usage of lists, loops, conditionals, arithmetic operations, random number generation, and various methods provided by the `System` namespace.