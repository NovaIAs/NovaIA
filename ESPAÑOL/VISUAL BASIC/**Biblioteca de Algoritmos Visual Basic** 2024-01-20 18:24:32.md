```visual basic
Public Function Factorial(n As Long) As Long
  If n = 0 Then
    Factorial = 1
  Else
    Factorial = n * Factorial(n - 1)
  End If
End Function

Public Sub Fibonacci(n As Long)
  Dim fib(n As Long) As Long
  fib(0) = 0
  fib(1) = 1

  For i As Long = 2 To n
    fib(i) = fib(i - 1) + fib(i - 2)
  Next i

  For Each f In fib
    Debug.Print f
  Next f
End Sub

Public Sub Quicksort(arr() As Long, izq As Long, der As Long)
  If izq < der Then
    Dim pivote As Long = arr((izq + der) \ 2)
    Dim i As Long = izq
    Dim j As Long = der

    While i <= j
      While arr(i) < pivote
        i = i + 1
      Wend

      While arr(j) > pivote
        j = j - 1
      Wend

      If i <= j Then
        Dim temp As Long = arr(i)
        arr(i) = arr(j)
        arr(j) = temp

        i = i + 1
        j = j - 1
      End If
    Wend

    Quicksort arr, izq, j
    Quicksort arr, i, der
  End If
End Sub

Public Sub PrimalityTest(n As Long)
  If n < 2 Then
    Debug.Print "El número no es primo."
  ElseIf n = 2 Then
    Debug.Print "El número es primo."
  ElseIf n Mod 2 = 0 Then
    Debug.Print "El número no es primo."
  Else
    Dim i As Long
    For i = 3 To Sqr(n) Step 2
      If n Mod i = 0 Then
        Debug.Print "El número no es primo."
        Exit For
      End If
    Next i

    If i > Sqr(n) Then
      Debug.Print "El número es primo."
    End If
  End If
End Sub

Public Sub ReverseString(str As String)
  Dim reversed As String = ""

  For i As Long = Len(str) To 1 Step -1
    reversed = reversed & Mid(str, i, 1)
  Next i

  Debug.Print reversed
End Sub

Public Sub BinarySearch(arr() As Long, target As Long)
  Dim izq As Long = 0
  Dim der As Long = UBound(arr)

  While izq <= der
    Dim medio As Long = (izq + der) \ 2

    If arr(medio) = target Then
      Debug.Print "El elemento " & target & " se encontró en la posición " & medio & "."
      Exit While
    ElseIf arr(medio) < target Then
      izq = medio + 1
    Else
      der = medio - 1
    End If
  Wend

  If izq > der Then
    Debug.Print "El elemento no se encontró."
  End If
End Sub

Public Sub MergeSort(arr() As Long)
  If UBound(arr) > 0 Then
    Dim medio As Long = (UBound(arr) - LBound(arr)) \ 2

    Dim izq() As Long = Mid(arr, LBound(arr), medio)
    Dim der() As Long = Mid(arr, medio + 1, UBound(arr) - medio)

    MergeSort izq
    MergeSort der

    Dim i As Long = LBound(izq)
    Dim j As Long = LBound(der)
    Dim k As Long = LBound(arr)

    While i <= UBound(izq) And j <= UBound(der)
      If izq(i) < der(j) Then
        arr(k) = izq(i)
        i = i + 1
      Else
        arr(k) = der(j)
        j = j + 1
      End If

      k = k + 1
    Wend

    While i <= UBound(izq)
      arr(k) = izq(i)
      i = i + 1
      k = k + 1
    Wend

    While j <= UBound(der)
      arr(k) = der(j)
      j = j + 1
      k = k + 1
    Wend
  End If
End Sub

Public Sub BubbleSort(arr() As Long)
  For i As Long = 0 To UBound(arr) - 1
    For j As Long = i + 1 To UBound(arr)
      If arr(i) > arr(j) Then
        Dim temp As Long = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      End If
    Next j
  Next i
End Sub

Public Sub SelectionSort(arr() As Long)
  For i As Long = 0 To UBound(arr) - 1
    Dim min As Long = i

    For j As Long = i + 1 To UBound(arr)
      If arr(j) < arr(min) Then
        min = j
      End If
    Next j

    Dim temp As Long = arr(i)
    arr(i) = arr(min)
    arr(min) = temp
  Next i
End Sub

Public Sub InsertionSort(arr() As Long)
  For i As Long = 1 To UBound(arr)
    Dim actual As Long = arr(i)
    Dim j As Long = i - 1

    While j >= 0 And actual < arr(j)
      arr(j + 1) = arr(j)
      j = j - 1
    Wend

    arr(j + 1) = actual
  Next i
End Sub

Public Sub HeapSort(arr() As Long)
  ' Construir el heap.
  For i As Long = Int((UBound(arr) - 1) / 2) To 0 Step -1
    Heapify arr, i, UBound(arr)
  Next i

  ' Ordenar el heap.
  For i As Long = UBound(arr) To 1 Step -1
    Dim temp As Long = arr(i)
    arr(i) = arr(0)
    arr(0) = temp

    Heapify arr, 0, i - 1
  Next i
End Sub

Private Sub Heapify(arr() As Long, i As Long, n As Long)
  Dim largest As Long = i
  Dim left As Long = 2 * i + 1
  Dim right As Long = 2 * i + 2

  ' Encontrar el hijo más grande.
  If left <= n And arr(left) > arr(largest) Then
    largest = left
  End If

  If right <= n And arr(right) > arr(largest) Then
    largest = right
  End If

  ' Si el hijo más grande no es el padre, intercambiarlos.
  If largest <> i Then
    Dim temp As Long = arr(i)
    arr(i) = arr(largest)
    arr(largest) = temp

    ' Recrear el heap para el nuevo padre.
    Heapify arr, largest, n
  End If
End Sub
```

Explicación del código:

1. `Factorial`: Calcula el factorial de un número entero positivo utilizando la recursividad.

2. `Fibonacci`: Genera la serie de Fibonacci hasta un número entero positivo dado.

3. `Quicksort`: Implementa el algoritmo de ordenación rápida para ordenar una matriz de números enteros largos.

4. `PrimalityTest`: Comprueba si un número entero es primo o no.

5. `ReverseString`: Invierte el orden de los caracteres de una cadena de texto.

6. `BinarySearch`: Busca un elemento en una matriz ordenada utilizando la búsqueda binaria.

7. `MergeSort`: Ordena una matriz de números enteros largos utilizando el algoritmo de ordenación por mezcla.

8. `BubbleSort`: Ordena una matriz de números enteros largos utilizando el algoritmo de ordenación por burbujas.

9. `SelectionSort`: Ordena una matriz de números enteros largos utilizando el algoritmo de ordenación por selección.

10. `InsertionSort`: Ordena una matriz de números enteros largos utilizando el algoritmo de ordenación por inserción.

11. `HeapSort`: Ordena una matriz de números enteros largos utilizando el algoritmo de ordenación por montículos.

12. `Heapify`: Función auxiliar utilizada por el algoritmo de ordenación por montículos para mantener la propiedad del montículo.