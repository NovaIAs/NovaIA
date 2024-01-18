```visual basic
Public Function FindLargestNumberInArray(arr() As Integer) As Integer
    ' Initialize the largest number to the first element in the array
    Dim largestNumber As Integer = arr(0)

    ' Loop through the remaining elements in the array
    For i As Integer = 1 To arr.Length - 1
        ' If the current element is larger than the largest number, update the largest number
        If arr(i) > largestNumber Then largestNumber = arr(i)
    Next i

    ' Return the largest number
    Return largestNumber
End Function

Public Function FindSmallestNumberInArray(arr() As Integer) As Integer
    ' Initialize the smallest number to the first element in the array
    Dim smallestNumber As Integer = arr(0)

    ' Loop through the remaining elements in the array
    For i As Integer = 1 To arr.Length - 1
        ' If the current element is smaller than the smallest number, update the smallest number
        If arr(i) < smallestNumber Then smallestNumber = arr(i)
    Next i

    ' Return the smallest number
    Return smallestNumber
End Function

Public Function FindAverageOfArray(arr() As Integer) As Double
    ' Initialize the sum to 0
    Dim sum As Integer = 0

    ' Loop through the elements in the array and add them to the sum
    For i As Integer = 0 To arr.Length - 1
        sum += arr(i)
    Next i

    ' Calculate the average by dividing the sum by the number of elements in the array
    Dim average As Double = sum / arr.Length

    ' Return the average
    Return average
End Function

Public Function FindMedianOfArray(arr() As Integer) As Double
    ' Sort the array in ascending order
    Array.Sort(arr)

    ' If the array has an odd number of elements, the median is the middle element
    If arr.Length Mod 2 = 1 Then
        Return arr(arr.Length \ 2)
    Else
        ' If the array has an even number of elements, the median is the average of the two middle elements
        Return (arr(arr.Length \ 2 - 1) + arr(arr.Length \ 2)) / 2
    End If
End Function

Public Function FindModeOfArray(arr() As Integer) As Integer
    ' Create a dictionary to store the frequency of each element in the array
    Dim frequencyDictionary As New Dictionary(Of Integer, Integer)

    ' Loop through the elements in the array and update the frequency of each element
    For i As Integer = 0 To arr.Length - 1
        If frequencyDictionary.ContainsKey(arr(i)) Then
            frequencyDictionary(arr(i)) += 1
        Else
            frequencyDictionary.Add(arr(i), 1)
        End If
    Next i

    ' Find the element with the highest frequency
    Dim maxFrequency As Integer = 0
    Dim mode As Integer = 0
    For i As Integer = 0 To arr.Length - 1
        If frequencyDictionary(arr(i)) > maxFrequency Then
            mode = arr(i)
            maxFrequency = frequencyDictionary(arr(i))
        End If
    Next i

    ' Return the mode
    Return mode
End Function
```

This code contains five functions that perform different statistical calculations on an array of integers:

1. `FindLargestNumberInArray`: Finds the largest number in an array.
2. `FindSmallestNumberInArray`: Finds the smallest number in an array.
3. `FindAverageOfArray`: Finds the average of all the numbers in an array.
4. `FindMedianOfArray`: Finds the median of all the numbers in an array.
5. `FindModeOfArray`: Finds the mode of all the numbers in an array.

Each function takes an array of integers as input and returns the desired statistical value. The functions use efficient algorithms to perform the calculations, ensuring good performance even for large arrays.

Here is an example of how to use these functions:

```visual basic
Dim arr As Integer() = {1, 3, 5, 7, 9}

Dim largestNumber As Integer = FindLargestNumberInArray(arr)
Dim smallestNumber As Integer = FindSmallestNumberInArray(arr)
Dim average As Double = FindAverageOfArray(arr)
Dim median As Double = FindMedianOfArray(arr)
Dim mode As Integer = FindModeOfArray(arr)

Console.WriteLine($"Largest Number: {largestNumber}")
Console.WriteLine($"Smallest Number: {smallestNumber}")
Console.WriteLine($"Average: {average}")
Console.WriteLine($"Median: {median}")
Console.WriteLine($"Mode: {mode}")
```

Output:

```
Largest Number: 9
Smallest Number: 1
Average: 5
Median: 5
Mode: 1
```