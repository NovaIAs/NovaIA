```visual basic
Const MAX_SIZE = 1000
Dim myArray(MAX_SIZE) As Integer

' Initialize the array with random numbers
For i = 0 To MAX_SIZE - 1
    myArray(i) = Rnd() * 1000
Next i

' Sort the array in ascending order
Array.Sort(myArray)

' Find the median of the array
Dim median As Integer
If MAX_SIZE Mod 2 = 0 Then
    median = (myArray(MAX_SIZE \ 2) + myArray(MAX_SIZE \ 2 + 1)) \ 2
Else
    median = myArray(MAX_SIZE \ 2)
End If

' Find the mode of the array
Dim mode As Integer
Dim modeCount As Integer = 0
Dim currentCount As Integer = 0
Dim currentValue As Integer = myArray(0)

For i = 1 To MAX_SIZE - 1
    If myArray(i) = currentValue Then
        currentCount += 1
    Else
        If currentCount > modeCount Then
            modeCount = currentCount
            mode = currentValue
        End If
        currentValue = myArray(i)
        currentCount = 1
    End If
Next i

If currentCount > modeCount Then
    modeCount = currentCount
    mode = currentValue
End If

' Find the range of the array
Dim range As Integer
range = myArray(MAX_SIZE - 1) - myArray(0)

' Find the mean of the array
Dim mean As Double
For i = 0 To MAX_SIZE - 1
    mean += myArray(i)
Next i
mean = mean / MAX_SIZE

' Find the standard deviation of the array
Dim standardDeviation As Double
For i = 0 To MAX_SIZE - 1
    standardDeviation += (myArray(i) - mean) ^ 2
Next i
standardDeviation = Sqr(standardDeviation / (MAX_SIZE - 1))

' Print the results
Console.WriteLine("Median: " & median)
Console.WriteLine("Mode: " & mode)
Console.WriteLine("Range: " & range)
Console.WriteLine("Mean: " & mean)
Console.WriteLine("Standard Deviation: " & standardDeviation)

' Wait for the user to press a key
Console.ReadKey()
```

This code performs a variety of statistical calculations on a large array of random numbers. It first initializes the array with random numbers, then sorts it in ascending order. Next, it finds the median, mode, range, mean, and standard deviation of the array. Finally, it prints the results to the console.

Here is a brief explanation of each of the statistical calculations:

* **Median:** The median is the middle value of a dataset. In this case, it is the middle value of the sorted array.
* **Mode:** The mode is the value that occurs most frequently in a dataset. In this case, it is the value that occurs most frequently in the array.
* **Range:** The range is the difference between the largest and smallest values in a dataset. In this case, it is the difference between the largest and smallest values in the array.
* **Mean:** The mean is the average value of a dataset. In this case, it is the sum of all the values in the array divided by the number of values in the array.
* **Standard Deviation:** The standard deviation is a measure of how spread out the values in a dataset are. In this case, it is the square root of the variance, which is the average of the squared differences between each value in the array and the mean.