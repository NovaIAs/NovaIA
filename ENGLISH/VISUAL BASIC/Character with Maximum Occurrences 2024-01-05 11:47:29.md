```visual basic
Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        ' Declare variables
        Dim inputText As String = TextBox1.Text
        Dim outputText As String = ""
        Dim charArray() As Char = inputText.ToCharArray()
        Dim charCount As Integer = charArray.Length
        Dim charMap As Dictionary(Of Char, Integer) = New Dictionary(Of Char, Integer)()
        Dim maxChar As Char = ' '
        Dim maxCount As Integer = 0

        ' Count the occurrences of each character
        For i As Integer = 0 To charCount - 1
            Dim char As Char = charArray(i)
            If charMap.ContainsKey(char) Then
                charMap(char) += 1
            Else
                charMap.Add(char, 1)
            End If
        Next

        ' Find the character with the maximum occurrence
        For Each kvp As KeyValuePair(Of Char, Integer) In charMap
            If kvp.Value > maxCount Then
                maxCount = kvp.Value
                maxChar = kvp.Key
            End If
        Next

        ' Build the output text
        outputText = String.Format("The character '{0}' appears {1} times in the input text.", maxChar, maxCount)

        ' Display the output text
        TextBox2.Text = outputText
    End Sub
End Class
```

Explanation:

1. **Declare Variables**:
   - `inputText`: Stores the input text entered by the user.
   - `outputText`: Stores the result to be displayed.
   - `charArray`: Array of characters representing the input text.
   - `charCount`: Length of the input text.
   - `charMap`: Dictionary to store the count of each character in the input text.
   - `maxChar`: Stores the character with the maximum occurrence.
   - `maxCount`: Stores the maximum occurrence count.

2. **Count Character Occurrences**:
   - Iterate through each character in the input text using a `For` loop.
   - For each character, check if it exists in the `charMap` dictionary.
   - If the character exists, increment its count; otherwise, add it to the dictionary with a count of 1.

3. **Find Character with Maximum Occurrence**:
   - Iterate through the `charMap` dictionary using a `For Each` loop.
   - For each character, check if its count is greater than the current `maxCount`.
   - If the count is greater, update `maxCount` and `maxChar` with the new values.

4. **Build Output Text**:
   - Use the `String.Format` method to format the output text.
   - The output text includes the character with the maximum occurrence and its count.

5. **Display Output Text**:
   - Set the text of the `TextBox2` control to the `outputText` string.