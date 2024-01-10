```visual basic
Public Function PrimeFactors(number As Long) As Collection
    Dim factors As New Collection
    Dim divisor As Long

    divisor = 2
    While divisor * divisor <= number
        While number Mod divisor = 0
            factors.Add divisor
            number = number \ divisor
        Wend
        divisor = divisor + 1
    Wend

    If number > 1 Then factors.Add number

    PrimeFactors = factors
End Function

Sub PrintPrimeFactors(number As Long)
    Dim factors As Collection

    factors = PrimeFactors(number)

    If factors.Count = 0 Then
        MsgBox "The number " & number & " has no prime factors."
    Else
        MsgBox "The prime factors of " & number & " are: " & Join(factors, ", ")
    End If
End Sub

Private Function Join(collection As Collection, separator As String) As String
    Dim result As String

    For Each item In collection
        If result <> "" Then result = result & separator
        result = result & item
    Next item

    Join = result
End Function
```

This code calculates the prime factors of a given number and then displays them in a message box.

The PrimeFactors function takes a number as its input and returns a collection of the prime factors of that number. The function uses a while loop to iterate through all the numbers from 2 to the square root of the input number. For each number, the function checks if it divides evenly into the input number. If it does, the function adds the number to the collection of prime factors and divides the input number by the number. This process continues until the input number is 1, at which point the function returns the collection of prime factors.

The PrintPrimeFactors sub takes a number as its input and calls the PrimeFactors function to calculate the prime factors of that number. The sub then uses a message box to display the prime factors to the user.

The Join function takes a collection and a separator as its input and returns a string that contains all the items in the collection, separated by the separator. The function uses a for each loop to iterate through the collection and add each item to the result string. The function also adds the separator between each item.

Here is an example of how to use the code:

```visual basic
Sub Main()
    PrintPrimeFactors 12
    PrintPrimeFactors 17
    PrintPrimeFactors 100
End Sub
```

This code will display the following message boxes:

```
The prime factors of 12 are: 2, 2, 3
The prime factors of 17 are: 17
The prime factors of 100 are: 2, 2, 5, 5
```