```visual basic
Public Function Fibonacci(ByVal n As Integer) As Integer
    If n < 2 Then
        Return n
    Else
        Return Fibonacci(n - 1) + Fibonacci(n - 2)
    End If
End Function

Public Function Factorial(ByVal n As Integer) As Integer
    If n = 0 Then
        Return 1
    Else
        Return n * Factorial(n - 1)
    End If
End Function

Public Sub BubbleSort(ByRef arr() As Integer)
    Dim i As Integer, j As Integer, temp As Integer

    For i = 0 To arr.Length - 1
        For j = 0 To arr.Length - i - 1
            If arr(j) > arr(j + 1) Then
                temp = arr(j)
                arr(j) = arr(j + 1)
                arr(j + 1) = temp
            End If
        Next j
    Next i
End Sub

Public Sub SelectionSort(ByRef arr() As Integer)
    Dim i As Integer, j As Integer, minIndex As Integer, temp As Integer

    For i = 0 To arr.Length - 1
        minIndex = i

        For j = i + 1 To arr.Length - 1
            If arr(j) < arr(minIndex) Then
                minIndex = j
            End If
        Next j

        temp = arr(i)
        arr(i) = arr(minIndex)
        arr(minIndex) = temp
    Next i
End Sub

Public Sub InsertionSort(ByRef arr() As Integer)
    Dim i As Integer, j As Integer, key As Integer

    For i = 1 To arr.Length - 1
        key = arr(i)
        j = i - 1

        While j >= 0 And arr(j) > key
            arr(j + 1) = arr(j)
            j -= 1
        Wend

        arr(j + 1) = key
    Next i
End Sub

Public Sub MergeSort(ByRef arr() As Integer)
    If arr.Length < 2 Then
        Return
    Else
        Dim mid As Integer = arr.Length \ 2
        Dim leftArr() As Integer = New Integer(mid - 1) {}
        Dim rightArr() As Integer = New Integer(arr.Length - mid) {}

        For i = 0 To mid - 1
            leftArr(i) = arr(i)
        Next i

        For i = mid To arr.Length - 1
            rightArr(i - mid) = arr(i)
        Next i

        MergeSort(leftArr)
        MergeSort(rightArr)

        Merge(arr, leftArr, rightArr)
    End If
End Sub

Private Sub Merge(ByRef arr() As Integer, ByRef leftArr() As Integer, ByRef rightArr() As Integer)
    Dim i As Integer = 0, j As Integer = 0, k As Integer = 0

    While i < leftArr.Length And j < rightArr.Length
        If leftArr(i) <= rightArr(j) Then
            arr(k) = leftArr(i)
            i += 1
        Else
            arr(k) = rightArr(j)
            j += 1
        End If
        k += 1
    Wend

    While i < leftArr.Length
        arr(k) = leftArr(i)
        i += 1
        k += 1
    Wend

    While j < rightArr.Length
        arr(k) = rightArr(j)
        j += 1
        k += 1
    Wend
End Sub

Public Sub QuickSort(ByRef arr() As Integer)
    If arr.Length < 2 Then
        Return
    Else
        Dim pivotIndex As Integer = arr.Length \ 2
        Dim pivotValue As Integer = arr(pivotIndex)
        Dim leftArr() As Integer = {}
        Dim rightArr() As Integer = {}

        For i = 0 To arr.Length - 1
            If i <> pivotIndex Then
                If arr(i) < pivotValue Then
                    ReDim Preserve leftArr(leftArr.Length)
                    leftArr(leftArr.Length - 1) = arr(i)
                Else
                    ReDim Preserve rightArr(rightArr.Length)
                    rightArr(rightArr.Length - 1) = arr(i)
                End If
            End If
        Next i

        QuickSort(leftArr)
        QuickSort(rightArr)

        arr = leftArr & New Integer() {pivotValue} & rightArr
    End If
End Sub
```

This code includes several sorting algorithms:

- Fibonacci: This function calculates the nth Fibonacci number using recursion.
- Factorial: This function calculates the factorial of a number using recursion.
- BubbleSort, SelectionSort, InsertionSort: These functions implement three different sorting algorithms: Bubble Sort, Selection Sort, and Insertion Sort.
- MergeSort: This function implements the Merge Sort algorithm.
- QuickSort: This function implements the Quick Sort algorithm.

This code also demonstrates how to use ReDim Preserve to dynamically resize arrays in Visual Basic.