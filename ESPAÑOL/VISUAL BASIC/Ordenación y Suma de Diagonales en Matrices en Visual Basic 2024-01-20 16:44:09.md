```visual basic
Public Sub ComplejoVisualBasic()
    ' Variables locales
    Dim i As Long
    Dim j As Long
    Dim k As Long
    Dim matriz(10, 10) As Integer

    ' Se llena la matriz con números aleatorios
    For i = 1 To 10
        For j = 1 To 10
            matriz(i, j) = Int(Rnd() * 100)
        Next j
    Next i

    ' Se imprime la matriz original
    Debug.Print "Matriz original:"
    For i = 1 To 10
        For j = 1 To 10
            Debug.Print matriz(i, j);
        Next j
        Debug.Print
    Next i

    ' Se ordena la matriz en orden ascendente usando el método de selección
    For i = 1 To 10
        For j = 1 To 10
            k = i
            For l = i + 1 To 10
                If matriz(l, j) < matriz(k, j) Then k = l
            Next l
            If k <> i Then
                For m = 1 To 10
                    Swap matriz(i, m), matriz(k, m)
                Next m
            End If
        Next j
    Next i

    ' Se imprime la matriz ordenada
    Debug.Print "Matriz ordenada:"
    For i = 1 To 10
        For j = 1 To 10
            Debug.Print matriz(i, j);
        Next j
        Debug.Print
    Next i

    ' Se suman los valores de la diagonal principal de la matriz
    Dim sumaDiagPrincipal As Long
    sumaDiagPrincipal = 0
    For i = 1 To 10
        sumaDiagPrincipal += matriz(i, i)
    Next i

    ' Se imprime la suma de los valores de la diagonal principal
    Debug.Print "Suma de los valores de la diagonal principal:"
    Debug.Print sumaDiagPrincipal

    ' Se suman los valores de la diagonal secundaria de la matriz
    Dim sumaDiagSecundaria As Long
    sumaDiagSecundaria = 0
    For i = 1 To 10
        sumaDiagSecundaria += matriz(i, 11 - i)
    Next i

    ' Se imprime la suma de los valores de la diagonal secundaria
    Debug.Print "Suma de los valores de la diagonal secundaria:"
    Debug.Print sumaDiagSecundaria
End Sub
```

Este código realiza las siguientes tareas:

1. Llena una matriz de 10x10 con números aleatorios.
2. Imprime la matriz original.
3. Ordena la matriz en orden ascendente usando el método de selección.
4. Imprime la matriz ordenada.
5. Suma los valores de la diagonal principal de la matriz.
6. Imprime la suma de los valores de la diagonal principal.
7. Suma los valores de la diagonal secundaria de la matriz.
8. Imprime la suma de los valores de la diagonal secundaria.

El código usa un ciclo `For` anidado para llenar la matriz con números aleatorios. Luego, usa otro ciclo `For` anidado para ordenar la matriz en orden ascendente usando el método de selección. El método de selección funciona encontrando el elemento más pequeño de la columna actual y luego intercambiándolo con el elemento en la fila actual. Esto se repite para cada columna de la matriz.

Una vez que la matriz está ordenada, el código suma los valores de la diagonal principal y secundaria de la matriz. La diagonal principal es la diagonal que va desde la esquina superior izquierda de la matriz hasta la esquina inferior derecha. La diagonal secundaria es la diagonal que va desde la esquina superior derecha de la matriz hasta la esquina inferior izquierda.

El código finalmente imprime la suma de los valores de la diagonal principal y secundaria de la matriz.