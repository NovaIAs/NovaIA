```visual basic
' Este código genera una matriz de números aleatorios y luego la ordena.

' Declaración de variables.
Dim matriz() As Integer
Dim tamaño As Integer = 10 ' Tamaño de la matriz.

' Generación de números aleatorios.
For i = 0 To tamaño - 1
    matriz(i) = Int(Rnd() * 100)
Next i

' Ordenación de la matriz.
For i = 0 To tamaño - 1
    For j = i + 1 To tamaño - 1
        If matriz(i) > matriz(j) Then
            Dim temp As Integer = matriz(i)
            matriz(i) = matriz(j)
            matriz(j) = temp
        End If
    Next j
Next i

' Impresión de la matriz ordenada.
For i = 0 To tamaño - 1
    Debug.Print matriz(i)
Next i
```

**Explicación del código:**

1. Declaramos las variables que utilizaremos:
    * **matriz() As Integer**: Es la matriz que contendrá los números aleatorios.
    * **tamaño As Integer = 10**: Es el tamaño de la matriz.

2. Generamos números aleatorios y los almacenamos en la matriz:
    * Utilizamos el bucle **For** para recorrer la matriz y generar un número aleatorio para cada elemento.
    * La función **Rnd()** genera un número aleatorio entre 0 y 1.
    * Multiplicamos el número aleatorio por 100 para obtener un número aleatorio entre 0 y 99.
    * Utilizamos la función **Int()** para convertir el número aleatorio a un número entero.

3. Ordenamos la matriz:
    * Utilizamos el algoritmo de ordenación por burbuja para ordenar la matriz.
    * El bucle **For** externo recorre la matriz desde el principio hasta el final.
    * El bucle **For** interno recorre la matriz desde el elemento actual hasta el final.
    * Si el elemento actual es mayor que el siguiente elemento, intercambiamos los dos elementos.

4. Imprimimos la matriz ordenada:
    * Utilizamos el bucle **For** para recorrer la matriz e imprimir cada elemento.
    * Utilizamos el método **Debug.Print** para imprimir los elementos de la matriz en la ventana de salida.