```visual basic
' Definir la función que calculará el factorial de un número
Function Factorial(número)
    ' Si el número es 0, devolver 1 (caso base)
    If número = 0 Then
        Factorial = 1
    ' En caso contrario, llamar recursivamente a la función con el número menos 1 y multiplicar el resultado por el número
    Else
        Factorial = número * Factorial(número - 1)
    End If
End Function

' Pedir al usuario que introduzca un número
InputBox "Introduzca un número:", "factorial"

' Calcular el factorial del número
resultado = Factorial(número)

' Mostrar el resultado al usuario
MsgBox "El factorial de " & número & " es " & resultado

' Definir una función que determine si un número es primo
Function EsPrimo(número)
    ' Si el número es 1, devolver falso (no es primo)
    If número = 1 Then
        EsPrimo = False
    ' En caso contrario, comprobar si el número es divisible por algún número del 2 al número menos 1
    Else
        EsPrimo = True
        For i = 2 To número - 1
            If número Mod i = 0 Then
                EsPrimo = False
                Exit For
            End If
        Next i
    End If
End Function

' Pedir al usuario que introduzca un número
InputBox "Introduzca un número:", "primo"

' Determinar si el número es primo
resultado = EsPrimo(número)

' Mostrar el resultado al usuario
If resultado Then
    MsgBox "El número " & número & " es primo"
Else
    MsgBox "El número " & número & " no es primo"
End If

' Definir una función que calcule la suma de los dígitos de un número
Function SumaDígitos(número)
    ' Inicializar la variable suma a 0
    suma = 0
    ' Convertir el número a una cadena de texto
    cadena = Str(número)
    ' Recorrer la cadena de texto y sumar cada dígito a la variable suma
    For i = 1 To Len(cadena)
        suma = suma + Val(Mid(cadena, i, 1))
    Next i
    ' Devolver la variable suma
    SumaDígitos = suma
End Function

' Pedir al usuario que introduzca un número
InputBox "Introduzca un número:", "suma de dígitos"

' Calcular la suma de los dígitos del número
resultado = SumaDígitos(número)

' Mostrar el resultado al usuario
MsgBox "La suma de los dígitos de " & número & " es " & resultado
```

Explicación del código:

* La primera parte del código define una función llamada `Factorial` que calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos desde 1 hasta ese número. Por ejemplo, el factorial de 5 es 5 x 4 x 3 x 2 x 1 = 120.
* La segunda parte del código define una función llamada `EsPrimo` que determina si un número es primo. Un número primo es un número entero positivo que solo tiene dos divisores: 1 y sí mismo. Por ejemplo, 7 es un número primo porque solo puede dividirse entre 1 y 7.
* La tercera parte del código define una función llamada `SumaDígitos` que calcula la suma de los dígitos de un número. Por ejemplo, la suma de los dígitos de 123 es 1 + 2 + 3 = 6.

El código utiliza estas funciones para calcular el factorial de un número, determinar si un número es primo y calcular la suma de los dígitos de un número. El código pide al usuario que introduzca un número y luego muestra el resultado de cada función para ese número.