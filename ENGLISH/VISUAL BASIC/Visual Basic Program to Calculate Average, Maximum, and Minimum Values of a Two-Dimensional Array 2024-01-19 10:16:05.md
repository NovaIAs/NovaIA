```visual basic
Option Explicit

' Declare the constants that will be used in the program.
Const NUM_ROWS = 10
Const NUM_COLS = 10

' Declare the two-dimensional array that will be used to store the data.
Dim arrData(NUM_ROWS, NUM_COLS) As Integer

' Populate the array with random data.
For i = 0 To NUM_ROWS - 1
    For j = 0 To NUM_COLS - 1
        arrData(i, j) = Rnd() * 100
    Next j
Next i

' Display the array to the user.
For i = 0 To NUM_ROWS - 1
    For j = 0 To NUM_COLS - 1
        Debug.Print arrData(i, j);
    Next j
    Debug.Print
Next i

' Calculate the average value of the array.
Dim dblAverage As Double
dblAverage = 0

For i = 0 To NUM_ROWS - 1
    For j = 0 To NUM_COLS - 1
        dblAverage = dblAverage + arrData(i, j)
    Next j
Next i

dblAverage = dblAverage / (NUM_ROWS * NUM_COLS)

' Display the average value to the user.
Debug.Print "The average value of the array is: " & dblAverage

' Calculate the maximum value of the array.
Dim intMax As Integer
intMax = 0

For i = 0 To NUM_ROWS - 1
    For j = 0 To NUM_COLS - 1
        If arrData(i, j) > intMax Then
            intMax = arrData(i, j)
        End If
    Next j
Next i

' Display the maximum value to the user.
Debug.Print "The maximum value of the array is: " & intMax

' Calculate the minimum value of the array.
Dim intMin As Integer
intMin = 100

For i = 0 To NUM_ROWS - 1
    For j = 0 To NUM_COLS - 1
        If arrData(i, j) < intMin Then
            intMin = arrData(i, j)
        End If
    Next j
Next i

' Display the minimum value to the user.
Debug.Print "The minimum value of the array is: " & intMin
```

This code creates a two-dimensional array of random numbers and then performs a number of calculations on the array, including calculating the average, maximum, and minimum values. The code is well-commented and easy to follow, making it a good example of complex and differentiated code.