```visual basic
Public Function IsPrime(ByVal number As Integer) As Boolean
    ' This function checks if the given number is prime

    ' Check if the number is 1, which is not prime
    If number = 1 Then Return False

    ' Initialize a variable to check divisors
    Dim divisor As Integer = 2

    ' Iterate until the divisor is less than or equal to the square root of the number
    Do While divisor * divisor <= number
        ' Check if the number is divisible by the current divisor
        If number Mod divisor = 0 Then
            ' If divisible, the number is not prime, so return False
            Return False
        End If

        ' Increment the divisor
        divisor = divisor + 1
    Loop

    ' If the loop completes without finding a divisor, the number is prime, so return True
    Return True
End Function

Public Sub FindPrimeFactors(ByVal number As Integer)
    ' This subroutine finds all the prime factors of the given number

    ' Initialize a variable to store the prime factors
    Dim primeFactors As New List(Of Integer)

    ' Iterate from 2 to the square root of the number
    For divisor As Integer = 2 To Math.Sqrt(number)

        ' Check if the number is divisible by the current divisor
        While number Mod divisor = 0
            ' Add the divisor to the list of prime factors
            primeFactors.Add(divisor)

            ' Divide the number by the divisor to get the next possible factor
            number = number \ divisor
        Wend
    Next

    ' If the number is greater than 1, it means it is a prime factor itself
    If number > 1 Then primeFactors.Add(number)

    ' Display the list of prime factors
    Console.WriteLine("Prime factors of {0}:", number)
    For Each factor In primeFactors
        Console.Write("{0} ", factor)
    Next
    Console.WriteLine()
End Sub

Public Sub Main()
    ' Get a number from the user
    Console.Write("Enter a positive integer: ")
    Dim number As Integer = Integer.Parse(Console.ReadLine())

    ' Check if the number is prime
    If IsPrime(number) Then
        Console.WriteLine("{0} is a prime number.", number)
    Else
        Console.WriteLine("{0} is not a prime number.", number)
    End If

    ' Find the prime factors of the number
    FindPrimeFactors(number)
End Sub
```

Explanation:

1. `IsPrime` Function:

   - This function checks if the given number is prime.

   - It starts by checking if the number is 1, which is not prime.

   - It then iterates through all numbers from 2 to the square root of the given number, checking if the number is divisible by any of these divisors.

   - If the number is divisible by any divisor, it returns False (indicating that the number is not prime).

   - If the loop completes without finding any divisors, it returns True (indicating that the number is prime).

2. `FindPrimeFactors` Subroutine:

   - This subroutine finds all the prime factors of the given number.

   - It starts by initializing an empty list to store the prime factors.

   - It then iterates through all numbers from 2 to the square root of the given number.

   - For each divisor, it checks if the given number is divisible by the divisor. If it is, it adds the divisor to the list of prime factors and divides the number by the divisor to get the next possible factor.

   - If the loop completes without finding any more divisors, it means that the given number is itself a prime factor, so it adds the number to the list of prime factors.

   - Finally, it displays the list of prime factors.

3. `Main` Subroutine:

   - This subroutine is the entry point of the program.

   - It gets a positive integer from the user and stores it in the `number` variable.

   - It calls the `IsPrime` function to check if the number is prime and displays the result.

   - It then calls the `FindPrimeFactors` subroutine to find and display the prime factors of the number.