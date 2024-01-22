```visual basic
' Declaramos los módulos necesarios.
Imports System
Imports System.Text.RegularExpressions

' Definimos la clase principal.
Public Class Main

    ' Definimos el método principal.
    Public Shared Sub Main(args As String())

        ' Definimos una variable para almacenar el texto de entrada.
        Dim inputText As String = "Esta es una cadena de entrada compleja con muchos números, símbolos y letras."

        ' Usamos una expresión regular para extraer los números de la cadena de entrada.
        Dim regex = New Regex("([0-9]+)")

        ' Almacenamos los números extraídos en una lista.
        Dim numbers = regex.Matches(inputText)

        ' Imprimimos la lista de números en la consola.
        For Each number In numbers
            Console.WriteLine(number.Value)
        Next

        ' Usamos una expresión regular para extraer las letras de la cadena de entrada.
        regex = New Regex("([a-zA-Z]+)")

        ' Almacenamos las letras extraídas en una lista.
        Dim letters = regex.Matches(inputText)

        ' Imprimimos la lista de letras en la consola.
        For Each letter In letters
            Console.WriteLine(letter.Value)
        Next

        ' Usamos una expresión regular para extraer los símbolos de la cadena de entrada.
        regex = New Regex("([.,!?;:])")

        ' Almacenamos los símbolos extraídos en una lista.
        Dim symbols = regex.Matches(inputText)

        ' Imprimimos la lista de símbolos en la consola.
        For Each symbol In symbols
            Console.WriteLine(symbol.Value)
        Next

    End Sub

End Class
```

Explicación del código:

* El código anterior define una clase llamada `Main` que contiene el método principal `Main`.
* El método `Main` es el punto de entrada del programa y se ejecuta cuando se ejecuta el programa.
* El código crea tres variables: `inputText`, `numbers`, `letters` y `symbols`.
* La variable `inputText` se utiliza para almacenar el texto de entrada.
* La variable `numbers` se utiliza para almacenar los números extraídos de la cadena de entrada.
* La variable `letters` se utiliza para almacenar las letras extraídas de la cadena de entrada.
* La variable `symbols` se utiliza para almacenar los símbolos extraídos de la cadena de entrada.
* El código utiliza tres expresiones regulares para extraer los números, las letras y los símbolos de la cadena de entrada.
* Las expresiones regulares se definen utilizando la clase `Regex`.
* La expresión regular para los números es `([0-9]+)`, que coincide con cualquier secuencia de dígitos.
* La expresión regular para las letras es `([a-zA-Z]+)`, que coincide con cualquier secuencia de letras.
* La expresión regular para los símbolos es `([.,!?;:])`, que coincide con cualquier símbolo de puntuación común.
* El código utiliza el método `Matches` de la clase `Regex` para extraer los números, las letras y los símbolos de la cadena de entrada.
* El método `Matches` devuelve una colección de objetos `Match`.
* El código utiliza el bucle `For Each` para recorrer la colección de objetos `Match` e imprimir los valores en la consola.