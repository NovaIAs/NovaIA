```visual basic
Sub ComplexVBCode()

    'Declare variables
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer

    'Create an array of arrays
    Dim TwoDimArray(10, 10) As Integer

    'Initialize the array with random values
    For i = 0 To 10
        For j = 0 To 10
            TwoDimArray(i, j) = Int((100 * Rnd()) + 1)
        Next j
    Next i

    'Find the maximum value in the array
    Dim maxValue As Integer = 0
    For i = 0 To 10
        For j = 0 To 10
            If TwoDimArray(i, j) > maxValue Then
                maxValue = TwoDimArray(i, j)
            End If
        Next j
    Next i

    'Find the minimum value in the array
    Dim minValue As Integer = 100
    For i = 0 To 10
        For j = 0 To 10
            If TwoDimArray(i, j) < minValue Then
                minValue = TwoDimArray(i, j)
            End If
        Next j
    Next i

    'Calculate the average value in the array
    Dim total As Integer = 0
    For i = 0 To 10
        For j = 0 To 10
            total = total + TwoDimArray(i, j)
        Next j
    Next i
    Dim averageValue As Integer = total / (10 * 10)

    'Display the results
    Debug.Print "Maximum value: " & maxValue
    Debug.Print "Minimum value: " & minValue
    Debug.Print "Average value: " & averageValue

End Sub
```

Explanation:

1. Declare variables: Declare several variables, including integers i, j, k, and arrays for storing values.

2. Create an array of arrays: TwoDimArray is created as a 10x10 array of integers to store values.

3. Initialize array with random values: Use a nested loop to fill the array with random values between 1 and 100.

4. Find maximum and minimum values: Nested loops are used to find the maximum (maxValue) and minimum (minValue) values within the array.

5. Calculate average value: Use nested loops to sum up all the values in the array (total) and then divide it by the total number of elements to get the average value (averageValue).

6. Display the results: Use Debug.Print statements to display the maximum, minimum, and average values to the Visual Basic debug output window.