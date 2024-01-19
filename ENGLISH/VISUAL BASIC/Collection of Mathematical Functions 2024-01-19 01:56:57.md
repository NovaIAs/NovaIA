```visual basic
Public Function Fibonacci(n As Integer) As Long
    If n = 0 Or n = 1 Then
        Fibonacci = 1
    Else
        Fibonacci = Fibonacci(n - 1) + Fibonacci(n - 2)
    End If
End Function

Public Function Factorial(n As Integer) As Long
    If n = 0 Then
        Factorial = 1
    Else
        Factorial = n * Factorial(n - 1)
    End If
End Function

Public Function IsPrime(n As Integer) As Boolean
    If n < 2 Then
        IsPrime = False
    Else
        Dim i As Integer
        For i = 2 To n - 1
            If n Mod i = 0 Then
                IsPrime = False
                Exit For
            End If
        Next i
        IsPrime = True
    End If
End Function

Public Function GreatestCommonDivisor(a As Integer, b As Integer) As Integer
    If b = 0 Then
        GreatestCommonDivisor = a
    Else
        GreatestCommonDivisor = GreatestCommonDivisor(b, a Mod b)
    End If
End Function

Public Function LeastCommonMultiple(a As Integer, b As Integer) As Integer
    LeastCommonMultiple = (a * b) \ GreatestCommonDivisor(a, b)
End Function

Public Function SortArray(arr() As Variant) As Variant()
    Dim i As Integer, j As Integer
    For i = 0 To UBound(arr) - 1
        For j = i + 1 To UBound(arr)
            If arr(i) > arr(j) Then
                Dim temp As Variant
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            End If
        Next j
    Next i
    SortArray = arr
End Function

Public Function BinarySearch(arr() As Variant, value As Variant) As Integer
    Dim low As Integer, high As Integer, mid As Integer
    low = 0
    high = UBound(arr)
    While low <= high
        mid = (low + high) \ 2
        If arr(mid) = value Then
            BinarySearch = mid
            Exit While
        ElseIf arr(mid) < value Then
            low = mid + 1
        Else
            high = mid - 1
        End If
    Wend
    If low > high Then
        BinarySearch = -1
    End If
End Function

Public Function MergeSort(arr() As Variant) As Variant()
    If UBound(arr) <= 0 Then
        MergeSort = arr
    Else
        Dim mid As Integer
        mid = (UBound(arr) + 1) \ 2
        Dim left() As Variant, right() As Variant
        left = MergeSort(Left(arr, mid - 1))
        right = MergeSort(Right(arr, UBound(arr) - mid + 1))
        MergeSort = Merge(left, right)
    End If
End Function

Public Function Merge(left() As Variant, right() As Variant) As Variant()
    Dim merged() As Variant, i As Integer, j As Integer, k As Integer
    ReDim merged(0 To UBound(left) + UBound(right))
    i = 0
    j = 0
    k = 0
    While i < UBound(left) And j < UBound(right)
        If left(i) <= right(j) Then
            merged(k) = left(i)
            i = i + 1
        Else
            merged(k) = right(j)
            j = j + 1
        End If
        k = k + 1
    Wend
    While i < UBound(left)
        merged(k) = left(i)
        i = i + 1
        k = k + 1
    Wend
    While j < UBound(right)
        merged(k) = right(j)
        j = j + 1
        k = k + 1
    Wend
    Merge = merged
End Function

Public Function QuickSort(arr() As Variant) As Variant()
    If UBound(arr) <= 0 Then
        QuickSort = arr
    Else
        Dim pivot As Variant, i As Integer, j As Integer
        pivot = arr(0)
        i = 1
        j = UBound(arr)
        While i <= j
            While i <= UBound(arr) And arr(i) <= pivot
                i = i + 1
            Wend
            While j >= 0 And arr(j) >= pivot
                j = j - 1
            Wend
            If i < j Then
                Dim temp As Variant
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
                i = i + 1
                j = j - 1
            Else
                Exit While
            End If
        Wend
        Dim left() As Variant, right() As Variant
        left = QuickSort(Left(arr, j))
        right = QuickS