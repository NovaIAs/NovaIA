```visual basic
Public Function CalcularFactorial(numero As Long) As Long
    Dim resultado As Long
    resultado = 1
    For i As Long = 1 To numero
        resultado = resultado * i
    Next i
    CalcularFactorial = resultado
End Function

Public Sub MayorMenor(numeros() As Integer)
    Dim mayor As Integer = numeros(0)
    Dim menor As Integer = numeros(0)

    For i As Long = 1 To UBound(numeros)
        If numeros(i) > mayor Then
            mayor = numeros(i)
        ElseIf numeros(i) < menor Then
            menor = numeros(i)
        End If
    Next i

    Console.WriteLine("El número mayor es: " & mayor)
    Console.WriteLine("El número menor es: " & menor)
End Sub

Public Sub OrdenarBurbuja(numeros() As Integer)
    Dim temp As Integer

    For i As Long = 0 To UBound(numeros)
        For j As Long = i + 1 To UBound(numeros)
            If numeros(i) > numeros(j) Then
                temp = numeros(i)
                numeros(i) = numeros(j)
                numeros(j) = temp
            End If
        Next j
    Next i
End Sub

Public Function BuscarBinaria(numeros() As Integer, numeroABuscar As Integer) As Integer
    Dim indiceInferior As Long = 0
    Dim indiceSuperior As Long = UBound(numeros)

    Do While indiceInferior <= indiceSuperior
        Dim indiceMedio As Long = (indiceInferior + indiceSuperior) \ 2
        Dim numeroMedio As Integer = numeros(indiceMedio)

        If numeroMedio = numeroABuscar Then
            BuscarBinaria = indiceMedio
            Exit Do
        ElseIf numeroMedio < numeroABuscar Then
            indiceInferior = indiceMedio + 1
        Else
            indiceSuperior = indiceMedio - 1
        End If
    Loop

    BuscarBinaria = -1 'No se encontró el número
End Function

Public Sub Main()
    Dim numeros() As Integer = {5, 3, 8, 2, 1, 4, 7, 6}
    Console.WriteLine("Factorial de 5: " & CalcularFactorial(5))
    MayorMenor(numeros)
    OrdenarBurbuja(numeros)
    Console.WriteLine("Números ordenados: " & Join(numeros, ", "))
    Dim indiceEncontrado As Integer = BuscarBinaria(numeros, 8)
    If indiceEncontrado >= 0 Then
        Console.WriteLine("El número 8 se encuentra en la posición " & indiceEncontrado)
    Else
        Console.WriteLine("El número 8 no se encontró")
    End If
End Sub
```

Explicación del código:

* La función `CalcularFactorial` calcula el factorial de un número dado.
* El procedimiento `MayorMenor` encuentra y muestra el mayor y el menor número de una matriz de enteros.
* El procedimiento `OrdenarBurbuja` ordena una matriz de enteros en orden ascendente utilizando el algoritmo de ordenación por burbuja.
* La función `BuscarBinaria` busca un número en una matriz de enteros ordenada utilizando el algoritmo de búsqueda binaria.
* El procedimiento `Main` muestra el factorial de 5, el mayor y el menor número de una matriz de enteros, la matriz ordenada y la posición de un número buscado en la matriz.