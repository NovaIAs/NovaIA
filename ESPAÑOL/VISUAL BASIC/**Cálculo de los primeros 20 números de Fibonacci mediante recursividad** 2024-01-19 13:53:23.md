```visual basic
Public Function Fibonacci(n As Integer) As Long
    ' Esta función calcula el n-ésimo número de Fibonacci de forma recursiva.

    If n = 0 Then
        Return 0
    ElseIf n = 1 Then
        Return 1
    Else
        Return Fibonacci(n - 1) + Fibonacci(n - 2)
    End If
End Function

Public Sub Main()
    ' Esta subrutina muestra los primeros 20 números de Fibonacci.

    Dim i As Integer
    For i = 0 To 19
        Console.WriteLine("Fibonacci({0}) = {1}", i, Fibonacci(i))
    Next
End Sub
```

Este código calcula los primeros 20 números de Fibonacci de forma recursiva. La función `Fibonacci` calcula el n-ésimo número de Fibonacci de forma recursiva. Si `n` es 0 o 1, la función devuelve 0 o 1, respectivamente. De lo contrario, la función llama recursivamente a sí misma con `n - 1` y `n - 2`, y devuelve la suma de los resultados de estas llamadas recursivas.

La subrutina `Main` llama a la función `Fibonacci` para calcular los primeros 20 números de Fibonacci. A continuación, muestra estos números en la consola.