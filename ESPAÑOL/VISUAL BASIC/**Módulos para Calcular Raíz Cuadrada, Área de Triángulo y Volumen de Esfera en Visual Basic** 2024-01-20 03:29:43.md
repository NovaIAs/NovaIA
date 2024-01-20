```visual basic
' Módulo para calcular la raíz cuadrada de un número real.

' Declaración de la función RaizCuadrada.
Function RaizCuadrada(numero As Double) As Double
    If numero < 0 Then
        MsgBox "No se puede calcular la raíz cuadrada de un número negativo."
        Exit Function
    End If

    ' Inicialización de la raíz cuadrada.
    Dim raiz As Double = numero / 2

    ' Iteración para calcular la raíz cuadrada.
    Do While Abs(raiz * raiz - numero) > 0.0001
        raiz = (raiz + numero / raiz) / 2
    Loop

    ' Retorno de la raíz cuadrada.
    RaizCuadrada = raiz
End Function

' Módulo para calcular el área de un triángulo.

' Declaración de la función AreaTriangulo.
Function AreaTriangulo(base As Double, altura As Double) As Double
    If base <= 0 Or altura <= 0 Then
        MsgBox "La base y la altura del triángulo deben ser mayores que cero."
        Exit Function
    End If

    ' Cálculo del área del triángulo.
    Dim area As Double = (base * altura) / 2

    ' Retorno del área del triángulo.
    AreaTriangulo = area
End Function

' Módulo para calcular el volumen de una esfera.

' Declaración de la función VolumenEsfera.
Function VolumenEsfera(radio As Double) As Double
    If radio <= 0 Then
        MsgBox "El radio de la esfera debe ser mayor que cero."
        Exit Function
    End If

    ' Cálculo del volumen de la esfera.
    Dim volumen As Double = (4 / 3) * Math.PI * radio ^ 3

    ' Retorno del volumen de la esfera.
    VolumenEsfera = volumen
End Function
```

El código anterior contiene tres módulos para calcular la raíz cuadrada de un número real, el área de un triángulo y el volumen de una esfera. Cada módulo contiene una función que realiza el cálculo correspondiente. Las funciones están declaradas con el modificador `Function` y tienen un tipo de retorno especificado. Los parámetros de las funciones están declarados con el tipo de dato correspondiente. Las funciones utilizan la instrucción `If` para comprobar si los parámetros de entrada son válidos y, en caso contrario, muestran un mensaje de error y salen de la función. Las funciones utilizan la instrucción `Do While` para iterar hasta que se cumple una condición determinada. Las funciones utilizan la instrucción `Return` para devolver el resultado del cálculo.