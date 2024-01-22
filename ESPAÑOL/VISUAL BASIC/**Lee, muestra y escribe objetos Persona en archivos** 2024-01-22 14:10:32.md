```visual basic
' Módulo principal: test

' Importa la biblioteca de entrada/salida para poder usar las funciones de lectura y escritura de archivos
Import System.IO

' Define una estructura llamada Persona que contiene los siguientes campos:
' - Nombre: El nombre de la persona
' - Apellido: El apellido de la persona
' - Edad: La edad de la persona
Structure Persona
    Public Nombre As String
    Public Apellido As String
    Public Edad As Integer
End Structure

' Define una función llamada LeerArchivo que toma una ruta de archivo como argumento y devuelve un array de objetos Persona
Function LeerArchivo(rutaArchivo As String) As Persona()
    ' Abre el archivo en modo de lectura
    Using lectorArchivo As StreamReader = New StreamReader(rutaArchivo)
        
        ' Lee la primera línea del archivo para obtener el número de personas
        Dim numeroPersonas As Integer = CInt(lectorArchivo.ReadLine())

        ' Crea un array de objetos Persona para almacenar las personas
        Dim personas As Persona() = New Persona(numeroPersonas - 1) {}

        ' Lee las líneas restantes del archivo y crea un objeto Persona para cada línea
        For i As Integer = 0 To numeroPersonas - 1
            ' Lee la línea actual del archivo
            Dim lineaActual As String = lectorArchivo.ReadLine()

            ' Divide la línea actual en tres partes: nombre, apellido y edad
            Dim partesLinea As String() = lineaActual.Split(",")

            ' Crea un objeto Persona con los datos de la línea actual
            Dim personaActual As Persona = New Persona() With {
                .Nombre = partesLinea(0),
                .Apellido = partesLinea(1),
                .Edad = CInt(partesLinea(2))
            }

            ' Añade el objeto Persona al array de personas
            personas(i) = personaActual
        Next

        ' Cierra el archivo
        lectorArchivo.Close()
    End Using

    ' Devuelve el array de personas
    Return personas
End Function

' Define una función llamada EscribirArchivo que toma una ruta de archivo y un array de objetos Persona como argumentos y escribe los objetos Persona en el archivo
Sub EscribirArchivo(rutaArchivo As String, personas As Persona())
    ' Abre el archivo en modo de escritura
    Using escritorArchivo As StreamWriter = New StreamWriter(rutaArchivo)

        ' Escribe el número de personas en la primera línea del archivo
        escritorArchivo.WriteLine(personas.Length)

        ' Escribe cada objeto Persona en una línea del archivo
        For Each persona In personas
            ' Escribe el nombre, el apellido y la edad de la persona en una línea del archivo
            escritorArchivo.WriteLine($"{persona.Nombre},{persona.Apellido},{persona.Edad}")
        Next

        ' Cierra el archivo
        escritorArchivo.Close()
    End Using
End Sub

' Define una función llamada MostrarPersonas que toma un array de objetos Persona como argumento y muestra las personas en la consola
Sub MostrarPersonas(personas As Persona())
    ' Recorre el array de personas y muestra cada persona en la consola
    For Each persona In personas
        ' Muestra el nombre, el apellido y la edad de la persona
        Console.WriteLine($"{persona.Nombre} {persona.Apellido} ({persona.Edad})")
    Next
End Sub

' Punto de entrada del programa
Sub Main()
    
    ' Obtiene la ruta del archivo de entrada del usuario
    Console.WriteLine("Escribe la ruta del archivo de entrada:")
    Dim rutaEntrada As String = Console.ReadLine()

    ' Obtiene la ruta del archivo de salida del usuario
    Console.WriteLine("Escribe la ruta del archivo de salida:")
    Dim rutaSalida As String = Console.ReadLine()

    ' Lee las personas del archivo de entrada
    Dim personas As Persona() = LeerArchivo(rutaEntrada)

    ' Muestra las personas en la consola
    MostrarPersonas(personas)

    ' Escribe las personas en el archivo de salida
    EscribirArchivo(rutaSalida, personas)

    ' Muestra un mensaje de confirmación
    Console.WriteLine("Personas escritas en el archivo de salida")
End Sub
```