```visual basic

' Definición de variables
Dim numero1 As Integer = 10
Dim numero2 As Integer = 20
Dim resultado As Integer

' Suma de dos números
resultado = numero1 + numero2
' Resta de dos números
resultado = numero1 - numero2
' Multiplicación de dos números
resultado = numero1 * numero2
' División de dos números
resultado = numero1 / numero2

' Declaración de una lista
Dim listaNumeros As List(Of Integer) = {1, 2, 3, 4, 5}

' Búsqueda de un elemento en la lista
Dim elementoBuscado As Integer = 3
Dim indiceElemento As Integer = listaNumeros.IndexOf(elementoBuscado)

' Condicional if-else
If indiceElemento > -1 Then
    Console.WriteLine("El elemento {0} se encuentra en la lista en la posición {1}", elementoBuscado, indiceElemento)
Else
    Console.WriteLine("El elemento {0} no se encuentra en la lista", elementoBuscado)
End If

' Bucle for
For numero As Integer = 1 To 10
    Console.WriteLine(numero)
Next

' Bucle while
Dim contador As Integer = 1
While contador <= 10
    Console.WriteLine(contador)
    contador += 1
End While

' Bucle do while
contador = 1
Do
    Console.WriteLine(contador)
    contador += 1
Loop While contador <= 10

' Función para calcular el cuadrado de un número
Function Cuadrado(numero As Integer) As Integer
    Return numero * numero
End Function

' Llamada a la función
Dim cuadradoNumero1 As Integer = Cuadrado(numero1)
Console.WriteLine("El cuadrado de {0} es {1}", numero1, cuadradoNumero1)

' Try-Catch para el manejo de errores
Try
    Dim resultadoDivision = numero1 / 0
    Console.WriteLine("El resultado de la división es {0}", resultadoDivision)
Catch ex As Exception
    Console.WriteLine("Se ha producido un error: {0}", ex.Message)
End Try

' Evento click para un botón
Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
    Console.WriteLine("Se ha hecho clic en el botón")
End Sub
```

Este código es una demostración de algunos de los conceptos básicos del lenguaje de programación Visual Basic. El código incluye:

* Definición de variables
* Suma, resta, multiplicación y división de dos números
* Declaración y uso de una lista
* Búsqueda de un elemento en una lista
* Condicional if-else
* Bucles for, while y do while
* Función para calcular el cuadrado de un número
* Llamada a la función
* Try-Catch para el manejo de errores
* Evento click para un botón

El código está bien documentado y es fácil de entender. Es un buen ejemplo de cómo utilizar Visual Basic para crear aplicaciones simples y complejas.