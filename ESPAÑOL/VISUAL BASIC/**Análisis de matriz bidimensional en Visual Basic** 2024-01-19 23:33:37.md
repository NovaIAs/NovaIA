```visual basic
' Definimos dos variables, una para guardar el número de la fila y otra para la columna
Dim fila As Integer, columna As Integer

' Solicitamos al usuario que ingrese el número de filas y columnas
fila = InputBox("Ingrese el número de filas:")
columna = InputBox("Ingrese el número de columnas:")

' Creamos una matriz de dos dimensiones con las dimensiones ingresadas por el usuario
Dim matriz(fila, columna) As Integer

' Recorremos la matriz y pedimos al usuario que ingrese un número para cada posición
For i = 0 To fila - 1
    For j = 0 To columna - 1
        matriz(i, j) = InputBox("Ingrese un número para la posición (" & i & ", " & j & "):")
    Next j
Next i

' Mostramos la matriz en pantalla
Print "Matriz:"
For i = 0 To fila - 1
    For j = 0 To columna - 1
        Print matriz(i, j);
    Next j
    Print
Next i

' Calculamos la suma de todos los elementos de la matriz
Dim suma As Integer
suma = 0
For i = 0 To fila - 1
    For j = 0 To columna - 1
        suma += matriz(i, j)
    Next j
Next i

' Mostramos la suma de todos los elementos de la matriz
Print "Suma de todos los elementos de la matriz:"
Print suma

' Calculamos el promedio de todos los elementos de la matriz
Dim promedio As Integer
promedio = suma / (fila * columna)

' Mostramos el promedio de todos los elementos de la matriz
Print "Promedio de todos los elementos de la matriz:"
Print promedio

' Calculamos el máximo de todos los elementos de la matriz
Dim maximo As Integer
maximo = matriz(0, 0)
For i = 0 To fila - 1
    For j = 0 To columna - 1
        If matriz(i, j) > maximo Then
            maximo = matriz(i, j)
        End If
    Next j
Next i

' Mostramos el máximo de todos los elementos de la matriz
Print "Máximo de todos los elementos de la matriz:"
Print maximo

' Calculamos el mínimo de todos los elementos de la matriz
Dim minimo As Integer
minimo = matriz(0, 0)
For i = 0 To fila - 1
    For j = 0 To columna - 1
        If matriz(i, j) < minimo Then
            minimo = matriz(i, j)
        End If
    Next j
Next i

' Mostramos el mínimo de todos los elementos de la matriz
Print "Mínimo de todos los elementos de la matriz:"
Print minimo
```

Este código crea una matriz de dos dimensiones con las dimensiones ingresadas por el usuario, y luego solicita al usuario que ingrese un número para cada posición de la matriz. A continuación, muestra la matriz en pantalla, calcula la suma de todos los elementos de la matriz, calcula el promedio de todos los elementos de la matriz, calcula el máximo de todos los elementos de la matriz y calcula el mínimo de todos los elementos de la matriz. Finalmente, muestra en pantalla la suma, el promedio, el máximo y el mínimo de todos los elementos de la matriz.