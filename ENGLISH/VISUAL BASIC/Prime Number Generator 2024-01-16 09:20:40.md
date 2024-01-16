```visual basic
Public Function IsPrime(ByVal number As Long) As Boolean
    ' Check if the number is less than or equal to 1.
    If number <= 1 Then
        ' Return False because numbers less than or equal to 1 are not prime.
        Return False
    End If

    ' Iterate through all numbers from 2 to the square root of the input number.
    For i As Long = 2 To Math.Sqrt(number)
        ' Check if the input number is divisible by the current number.
        If number Mod i = 0 Then
            ' Return False because the input number is divisible by another number, so it is not prime.
            Return False
        End If
    Next

    ' If the loop completes without finding any divisors, the input number is prime.
    Return True
End Function

Public Sub PrimeNumbersUpTo(ByVal n As Long)
    ' Initialize a list to store the prime numbers.
    Dim primeNumbers As New List(Of Long)

    ' Iterate through all numbers from 2 to the input number.
    For i As Long = 2 To n
        ' Check if the current number is prime.
        If IsPrime(i) Then
            ' Add the current number to the list of prime numbers.
            primeNumbers.Add(i)
        End If
    Next

    ' Print the list of prime numbers.
    For Each primeNumber As Long In primeNumbers
        Console.WriteLine(primeNumber)
    Next
End Sub

' Call the PrimeNumbersUpTo method to find and print all prime numbers up to 100.
PrimeNumbersUpTo(100)
```

Explanation:

1. `IsPrime` Function:
   - This function checks if a given number is prime.
   - It iterates through all numbers from 2 to the square root of the input number to see if the input number is divisible by any of these numbers.
   - If the input number is divisible by any number other than 1 and itself, it is not prime, and the function returns False.
   - If the loop completes without finding any divisors, the input number is prime, and the function returns True.

2. `PrimeNumbersUpTo` Subroutine:
   - This subroutine finds and prints all prime numbers up to a specified number.
   - It initializes an empty list to store the prime numbers.
   - It iterates through all numbers from 2 to the input number and calls the `IsPrime` function to check if each number is prime.
   - If a number is prime, it is added to the list of prime numbers.
   - After the loop completes, the list of prime numbers is printed to the console.

3. Call to `PrimeNumbersUpTo`:
   - The `PrimeNumbersUpTo` subroutine is called with an input number of 100.
   - This will find and print all prime numbers up to 100.