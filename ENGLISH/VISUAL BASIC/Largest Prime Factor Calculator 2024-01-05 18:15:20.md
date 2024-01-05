```visual basic
Public Function FindLargestPrimeFactor(number As Long) As Long
    ' Initialize the largest prime factor to 1.
    Dim largestPrimeFactor As Long = 1

    ' Check if the number is divisible by 2.
    While number Mod 2 = 0
        ' If the number is divisible by 2, update the largest prime factor and divide the number by 2.
        largestPrimeFactor = 2
        number /= 2
    Wend

    ' Start checking for prime factors from 3 to the square root of the number.
    Dim i As Long = 3
    While i <= Sqr(number)
        ' Check if the number is divisible by the current value of i.
        While number Mod i = 0
            ' If the number is divisible by the current value of i, update the largest prime factor and divide the number by i.
            largestPrimeFactor = i
            number /= i
        Wend

        ' Increment the value of i by 2, since all even numbers greater than 2 are not prime.
        i += 2
    Wend

    ' If the number is greater than 1, then it is a prime number and is the largest prime factor of itself.
    If number > 1 Then
        largestPrimeFactor = number
    End If

    ' Return the largest prime factor.
    FindLargestPrimeFactor = largestPrimeFactor
End Function

Public Sub Main()
    ' Get the input number from the user.
    Console.WriteLine("Enter a number:")
    Dim number As Long = Console.ReadLine()

    ' Find the largest prime factor of the number.
    Dim largestPrimeFactor As Long = FindLargestPrimeFactor(number)

    ' Display the largest prime factor.
    Console.WriteLine("The largest prime factor of {0} is {1}", number, largestPrimeFactor)

    Console.ReadKey()
End Sub
```

This code snippet is a Visual Basic program that finds the largest prime factor of a given number. It uses a function called FindLargestPrimeFactor to find the largest prime factor of a given number. The function works by checking if the number is divisible by 2, and if it is, it updates the largest prime factor to 2 and divides the number by 2. If the number is not divisible by 2, it starts checking for prime factors from 3 to the square root of the number. If the number is divisible by the current prime factor, it updates the largest prime factor and divides the number by the prime factor. The function increments the prime factor by 2, since all even numbers greater than 2 are not prime. If the number is greater than 1 after the loop, then it is a prime number and is the largest prime factor of itself. The function returns the largest prime factor.

The Main subroutine gets the input number from the user and calls the FindLargestPrimeFactor function to find the largest prime factor of the number. It then displays the largest prime factor to the console.