```visual basic
Option Explicit

' Declare global variables
Dim strInput As String
Dim arrWords As Variant
Dim intWordCount As Integer
Dim strLongestWord As String
Dim intLongestWordLength As Integer
Dim strShortestWord As String
Dim intShortestWordLength As Integer
Dim strMostFrequentWord As String
Dim intMostFrequentWordCount As Integer
Dim strLeastFrequentWord As String
Dim intLeastFrequentWordCount As Integer

' Get input from user
strInput = InputBox("Enter a sentence:")

' Split input into words
arrWords = Split(strInput, " ")

' Count the number of words
intWordCount = UBound(arrWords) + 1

' Find the longest and shortest words
For i = 0 To intWordCount - 1
    If Len(arrWords(i)) > intLongestWordLength Then
        strLongestWord = arrWords(i)
        intLongestWordLength = Len(arrWords(i))
    End If
    If Len(arrWords(i)) < intShortestWordLength Or intShortestWordLength = 0 Then
        strShortestWord = arrWords(i)
        intShortestWordLength = Len(arrWords(i))
    End If
Next i

' Find the most and least frequent words
Dim dictWords As Scripting.Dictionary
Set dictWords = New Scripting.Dictionary
For i = 0 To intWordCount - 1
    If Not dictWords.Exists(arrWords(i)) Then
        dictWords.Add arrWords(i), 1
    Else
        dictWords(arrWords(i)) = dictWords(arrWords(i)) + 1
    End If
Next i

Dim strTempWord As String
Dim intTempCount As Integer
strMostFrequentWord = ""
intMostFrequentWordCount = 0
strLeastFrequentWord = ""
intLeastFrequentWordCount = intWordCount
For Each strTempWord In dictWords.Keys
    intTempCount = dictWords(strTempWord)
    If intTempCount > intMostFrequentWordCount Then
        strMostFrequentWord = strTempWord
        intMostFrequentWordCount = intTempCount
    End If
    If intTempCount < intLeastFrequentWordCount Then
        strLeastFrequentWord = strTempWord
        intLeastFrequentWordCount = intTempCount
    End If
Next strTempWord

' Display results
MsgBox "Number of words: " & intWordCount & vbCrLf & _
    "Longest word: " & strLongestWord & " (" & intLongestWordLength & " characters)" & vbCrLf & _
    "Shortest word: " & strShortestWord & " (" & intShortestWordLength & " characters)" & vbCrLf & _
    "Most frequent word: " & strMostFrequentWord & " (" & intMostFrequentWordCount & " occurrences)" & vbCrLf & _
    "Least frequent word: " & strLeastFrequentWord & " (" & intLeastFrequentWordCount & " occurrences)"

```

This code takes a sentence from the user and analyzes it to find the following information:

* The number of words in the sentence
* The longest and shortest words in the sentence
* The most and least frequent words in the sentence

The code first splits the input sentence into words using the `Split` function. Then, it counts the number of words in the sentence by finding the upper bound of the array of words.

Next, the code finds the longest and shortest words in the sentence by iterating through the array of words and comparing the length of each word to the current longest and shortest word lengths.

The code then finds the most and least frequent words in the sentence by using a `Scripting.Dictionary` object to store the words and their frequencies. The code iterates through the array of words and adds each word to the dictionary, incrementing its frequency if it already exists.

Finally, the code displays the results of the analysis in a message box.