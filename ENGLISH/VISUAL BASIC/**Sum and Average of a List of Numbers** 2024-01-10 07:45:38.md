```visual basic
' Declare variables
Dim strInput As String
Dim arrInput() As String
Dim intSum As Integer
Dim intAverage As Double
Dim intCount As Integer

' Get input from user
strInput = InputBox("Enter a list of numbers separated by commas:")

' Split the input string into an array of strings
arrInput = Split(strInput, ",")

' Initialize variables
intSum = 0
intAverage = 0
intCount = 0

' Iterate through the array of strings
For Each strNumber In arrInput
    ' Convert the string to an integer
    Dim intNumber As Integer = CInt(strNumber)

    ' Add the number to the sum
    intSum += intNumber

    ' Increment the count of numbers
    intCount += 1
Next

' Calculate the average
intAverage = intSum / intCount

' Display the results
MsgBox("The sum of the numbers is " & intSum)
MsgBox("The average of the numbers is " & intAverage)
```

This code performs the following steps:

1. Declare variables to store the input string, the array of numbers, the sum of the numbers, the average of the numbers, and the count of numbers.
2. Get input from the user using the InputBox function. The user is prompted to enter a list of numbers separated by commas.
3. Split the input string into an array of strings using the Split function. The comma character is used as the separator.
4. Initialize the sum, average, and count variables to zero.
5. Iterate through the array of strings using a For Each loop.
6. Convert each string in the array to an integer using the CInt function.
7. Add the number to the sum.
8. Increment the count of numbers.
9. Calculate the average by dividing the sum by the count.
10. Display the results using the MsgBox function. The sum and average of the numbers are displayed in separate message boxes.