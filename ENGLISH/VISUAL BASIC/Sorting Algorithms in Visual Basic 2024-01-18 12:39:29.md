```visual basic
Public Function BubbleSort(ByRef arr() As Variant) As Integer
  Dim i As Integer
  Dim j As Integer
  Dim temp As Variant
  For i = 0 To UBound(arr, 1) - 1
    For j = i + 1 To UBound(arr, 1)
      If arr(i) > arr(j) Then
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      End If
    Next j
  Next i
  BubbleSort = 1
End Function

Public Function SelectionSort(ByRef arr() As Variant) As Integer
  Dim i As Integer
  Dim j As Integer
  Dim min As Variant
  For i = 0 To UBound(arr, 1) - 1
    min = i
    For j = i + 1 To UBound(arr, 1)
      If arr(j) < arr(min) Then min = j
    Next j
    If min <> i Then
      temp = arr(i)
      arr(i) = arr(min)
      arr(min) = temp
    End If
  Next i
  SelectionSort = 1
End Function

Public Function InsertionSort(ByRef arr() As Variant) As Integer
  Dim i As Integer
  Dim j As Integer
  Dim temp As Variant
  For i = 1 To UBound(arr, 1)
    temp = arr(i)
    j = i - 1
    While j >= 0 And arr(j) > temp
      arr(j + 1) = arr(j)
      j = j - 1
    Wend
    arr(j + 1) = temp
  Next i
  InsertionSort = 1
End Function

Public Function QuickSort(ByRef arr() As Variant, ByVal first As Integer, ByVal last As Integer) As Integer
  If first < last Then
    Dim pivot As Integer
    Dim i As Integer
    Dim j As Integer
    pivot = arr(last)
    i = first - 1
    For j = first To last - 1
      If arr(j) < pivot Then
        i = i + 1
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      End If
    Next j
    arr(last) = arr(i + 1)
    arr(i + 1) = pivot
    QuickSort arr, first, i
    QuickSort arr, i + 2, last
  End If
  QuickSort = 1
End Function

Public Function MergeSort(ByRef arr() As Variant, ByVal first As Integer, ByVal last As Integer) As Integer
  If first < last Then
    Dim mid As Integer
    mid = (first + last) \ 2
    MergeSort arr, first, mid
    MergeSort arr, mid + 1, last
    Merge arr, first, mid, last
  End If
  MergeSort = 1
End Function

Private Sub Merge(ByRef arr() As Variant, ByVal first As Integer, ByVal mid As Integer, ByVal last As Integer)
  Dim temp() As Variant
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  ReDim temp(last - first + 1)
  i = first
  j = mid + 1
  k = 0
  While i <= mid And j <= last
    If arr(i) < arr(j) Then
      temp(k) = arr(i)
      i = i + 1
    Else
      temp(k) = arr(j)
      j = j + 1
    End If
    k = k + 1
  Wend
  While i <= mid
    temp(k) = arr(i)
    i = i + 1
    k = k + 1
  Wend
  While j <= last
    temp(k) = arr(j)
    j = j + 1
    k = k +