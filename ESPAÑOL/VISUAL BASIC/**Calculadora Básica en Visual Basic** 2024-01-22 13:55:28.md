```visual basic
' Definir el procedimiento principal
Sub principal()
    ' Definir variables
    Dim numero1 As Integer
    Dim numero2 As Integer
    Dim resultado As Integer
    Dim operacion As String

    ' Solicitar al usuario que ingrese los números y la operación
    Console.WriteLine("Ingrese el primer número:")
    numero1 = Console.ReadLine()
    Console.WriteLine("Ingrese el segundo número:")
    numero2 = Console.ReadLine()
    Console.WriteLine("Ingrese la operación (+, -, *, /):")
    operacion = Console.ReadLine()

    ' Realizar la operación
    Select Case operacion
        Case "+"
            resultado = numero1 + numero2
        Case "-"
            resultado = numero1 - numero2
        Case "*"
            resultado = numero1 * numero2
        Case "/"
            If numero2 <> 0 Then
                resultado = numero1 / numero2
            Else
                Console.WriteLine("No se puede dividir por 0")
            End If
        Case Else
            Console.WriteLine("Operación no válida")
    End Select

    ' Mostrar el resultado
    If resultado IsNot Nothing Then
        Console.WriteLine("Resultado: " & resultado)
    End If
End Sub
```

El código anterior es un programa sencillo que solicita al usuario que ingrese dos números y una operación (+, -, *, /), y luego realiza la operación y muestra el resultado. El código utiliza la instrucción Select Case para realizar la operación, y la instrucción If para comprobar si el segundo número es 0 antes de realizar la división. El código también utiliza la instrucción Console.WriteLine para mostrar el resultado de la operación al usuario.

Aquí hay una explicación más detallada del código:

* La primera línea del código define el procedimiento principal del programa. En Visual Basic, el procedimiento principal siempre se llama "principal".
* Las siguientes líneas del código definen las variables que se utilizarán en el programa. En este caso, se definen tres variables: numero1, numero2 y resultado.
* Las siguientes líneas del código solicitan al usuario que ingrese los números y la operación. El método Console.ReadLine() se utiliza para leer la entrada del usuario.
* Las siguientes líneas del código utilizan la instrucción Select Case para realizar la operación. La instrucción Select Case evalúa el valor de la variable operacion y ejecuta el código correspondiente a la opción que coincida.
* Las siguientes líneas del código utilizan la instrucción If para comprobar si el segundo número es 0 antes de realizar la división. Si el segundo número es 0, se muestra un mensaje de error al usuario.
* Las siguientes líneas del código utilizan la instrucción Console.WriteLine para mostrar el resultado de la operación al usuario.

El código anterior es sólo un ejemplo sencillo de un programa de Visual Basic. Visual Basic es un lenguaje de programación potente que se puede utilizar para crear una amplia variedad de aplicaciones.