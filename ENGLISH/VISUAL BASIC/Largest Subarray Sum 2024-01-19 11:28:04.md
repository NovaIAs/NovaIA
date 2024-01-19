```visual basic
Public Function FindLargestSubarraySum(arr() As Integer) As Integer
    Dim currentSubarraySum = 0
    Dim largestSubarraySum = -999999999 'Initialize to a very low value

    For i As Integer = 0 To arr.Length - 1
        currentSubarraySum = currentSubarraySum + arr(i)

        If currentSubarraySum > largestSubarraySum Then
            largestSubarraySum = currentSubarraySum
        End If

        If currentSubarraySum < 0 Then
            currentSubarraySum = 0 'Reset current subarray sum if it becomes negative
        End If
    Next i

    Return largestSubarraySum
End Function

Dim testArray() As Integer = {1, -2, 3, 4, -5, 6, -7, 8, 9}
Dim result As Integer = FindLargestSubarraySum(testArray)

Console.WriteLine($"The largest subarray sum is {result}")
```

Explanation:

1. We define a function called `FindLargestSubarraySum` that takes an array of integers as input and returns the largest subarray sum.

2. Inside the function, we initialize two variables:
   - `currentSubarraySum` to store the sum of the current subarray.
   - `largestSubarraySum` to store the largest subarray sum found so far. We initialize it to a very low value to ensure that it is always updated with a larger value.

3. We iterate through the input array using a `For` loop. For each element, we add it to the `currentSubarraySum`.

4. We check if the `currentSubarraySum` is greater than the `largestSubarraySum`. If it is, we update `largestSubarraySum` to the value of `currentSubarraySum`.

5. We also check if the `currentSubarraySum` is less than 0. If it is, we reset `currentSubarraySum` to 0. This is because we only want to consider positive subarray sums.

6. After processing all elements in the array, we return `largestSubarraySum`.

7. In the `Main` method, we define a test array and call the `FindLargestSubarraySum` function to find the largest subarray sum. We then output the result to the console.