```visual basic
' Creamos una función que recibe un número y devuelve su factorial
Function factorial(numero)
    ' Si el número es 0 o 1, retornamos 1
    If numero = 0 Or numero = 1 Then
        Return 1
    ' Si no, calculamos el factorial recursivamente
    Else
        Return numero * factorial(numero - 1)
    End If
End Function

' Creamos una función que recibe un número y devuelve si es primo
Function esPrimo(numero)
    ' Si el número es 0 o 1, no es primo
    If numero = 0 Or numero = 1 Then
        Return False
    ' Si el número es 2, es primo
    ElseIf numero = 2 Then
        Return True
    ' Si no, comprobamos si es divisible entre algún número entre 2 y la raíz cuadrada del número
    Else
        For i = 2 To Sqr(numero)
            If numero Mod i = 0 Then
                Return False
            End If
        Next i
    End If
    ' Si no es divisible entre ningún número, es primo
    Return True
End Function

' Creamos una función que recibe un número y devuelve su serie de Fibonacci
Function fibonacci(numero)
    ' Si el número es 0 o 1, retornamos ese número
    If numero = 0 Or numero = 1 Then
        Return numero
    ' Si no, calculamos el número de Fibonacci recursivamente
    Else
        Return fibonacci(numero - 1) + fibonacci(numero - 2)
    End If
End Function

' Creamos una función que recibe un número y devuelve su número primo más cercano
Function primoMasCercano(numero)
    ' Si el número es primo, retornamos el mismo número
    If esPrimo(numero) Then
        Return numero
    ' Si no, buscamos el primo más cercano
    Else
        ' Buscamos el primo más cercano menor que el número
        Do While Not esPrimo(numero)
            numero -= 1
        Loop
        ' Buscamos el primo más cercano mayor que el número
        Do While Not esPrimo(numero)
            numero += 1
        Loop
        ' Retornamos el primo más cercano
        Return numero
    End If
End Function

' Creamos una función que recibe una lista de números y devuelve la media
Function media(lista)
    ' Si la lista está vacía, retornamos 0
    If lista.Count = 0 Then
        Return 0
    ' Si no, calculamos la media de la lista
    Else
        suma = 0
        For i = 0 To lista.Count - 1
            suma += lista(i)
        Next i
        media = suma / lista.Count
        Return media
    End If
End Function

' Creamos una función que recibe una lista de números y devuelve la desviación estándar
Function desviacionEstandar(lista)
    ' Si la lista está vacía, retornamos 0
    If lista.Count = 0 Then
        Return 0
    ' Si no, calculamos la desviación estándar de la lista
    Else
        media = media(lista)
        varianza = 0
        For i = 0 To lista.Count - 1
            varianza += (lista(i) - media) ^ 2
        Next i
        varianza /= lista.Count - 1
        desviacionEstandar = Sqr(varianza)
        Return desviacionEstandar
    End If
End Function

' Creamos una función que recibe una lista de números y devuelve el elemento más frecuente
Function elementoMasFrecuente(lista)
    ' Si la lista está vacía, retornamos 0
    If lista.Count = 0 Then
        Return 0
    ' Si no, calculamos el elemento más frecuente
    Else
        diccionario = CreateObject("Scripting.Dictionary")
        For i = 0 To lista.Count