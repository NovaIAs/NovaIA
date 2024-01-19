```visual basic
' Declare variables
Dim i As Long, j As Long, k As Long, l As Long
Dim strInput As String, strOutput As String
Dim arrInput() As String, arrOutput() As String

' Get input from the user
strInput = InputBox("Enter a string:")

' Split the input string into an array of words
arrInput = Split(strInput, " ")

' Create an array to store the output
ReDim arrOutput(0 To UBound(arrInput) * 2)

' Loop through the input array
For i = 0 To UBound(arrInput)
    ' Loop through each character in the word
    For j = 0 To Len(arrInput(i)) - 1
        ' Get the ASCII code of the character
        k = Asc(Mid(arrInput(i), j + 1, 1))

        ' If the ASCII code is even, add the character to the output array
        If k Mod 2 = 0 Then
            arrOutput(l) = Mid(arrInput(i), j + 1, 1)
            l = l + 1
        End If
    Next j

    ' Add a space to the output array
    arrOutput(l) = " "
    l = l + 1
Next i

' Join the output array into a string
strOutput = Join(arrOutput, "")

' Display the output
MsgBox strOutput
```

**Explanation:**

This code takes a string from the user and then splits it into an array of words. It then loops through the array of words and loops through each character in each word. For each character, it gets the ASCII code and checks if it is even. If it is, the character is added to an array. After all the characters in the word have been processed, a space is added to the array. This process is repeated for all the words in the input string.

Once all the words have been processed, the array is joined into a string and displayed in a message box.

This code is complex because it involves multiple loops and conditional statements. It also uses the Split function to split the input string into an array of words and the Join function to join the array of characters back into a string.