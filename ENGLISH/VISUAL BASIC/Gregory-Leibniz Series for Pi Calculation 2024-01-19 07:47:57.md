```visual basic
Public Function CalculatePi(numberOfTerms As Long) As Double
    ' Initialize variables
    Dim denominator As Double = 1
    Dim pi As Double = 0
    Dim sign As Integer = 1

    ' Loop through the terms
    For i As Long = 1 To numberOfTerms
        ' Calculate the next term
        Dim term As Double = sign / denominator

        ' Add the term to the pi approximation
        pi += term

        ' Update the denominator and sign for the next term
        denominator += 2
        sign *= -1
    Next i

    ' Return the pi approximation
    Return pi
End Function

Public Sub Main()
    ' Calculate pi to 10000 terms
    Dim pi As Double = CalculatePi(10000)

    ' Display the result
    MsgBox("Pi = " & pi)
End Sub
```

The code above calculates the value of pi using the Gregory-Leibniz series. The Gregory-Leibniz series is an infinite series that converges to the value of pi. The formula for the series is:

```
pi = 4 * (1 - 1/3 + 1/5 - 1/7 + ...)
```

The code above calculates the value of pi by summing the terms of this series up to a specified number of terms. The number of terms is specified by the `numberOfTerms` parameter.

The code first initializes the variables that are used in the calculation. The `denominator` variable is used to keep track of the denominator of the current term in the series. The `pi` variable is used to store the current approximation of the value of pi. The `sign` variable is used to keep track of the sign of the current term in the series.

The code then enters a loop that iterates through the terms of the series. In each iteration of the loop, the code calculates the next term in the series and adds it to the current approximation of the value of pi. The code also updates the `denominator` and `sign` variables for the next term in the series.

After the loop has completed, the code returns the final approximation of the value of pi.

The code then calls the `Main()` subroutine, which calculates the value of pi to 10000 terms and displays the result in a message box.