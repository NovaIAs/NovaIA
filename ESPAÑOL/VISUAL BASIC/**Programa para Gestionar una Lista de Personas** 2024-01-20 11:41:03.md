```visual basic
' Importamos las librerías necesarias.
Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Threading.Tasks

' Definimos una clase llamada "Persona".
Public Class Persona
    ' Propiedad "Nombre" de tipo String.
    Public Property Nombre As String

    ' Propiedad "Apellido" de tipo String.
    Public Property Apellido As String

    ' Propiedad "Edad" de tipo Integer.
    Public Property Edad As Integer

    ' Constructor de la clase "Persona".
    Public Sub New(nombre As String, apellido As String, edad As Integer)
        Me.Nombre = nombre
        Me.Apellido = apellido
        Me.Edad = edad
    End Sub

    ' Método "Saludar" que devuelve un String.
    Public Function Saludar() As String
        Return "Hola, mi nombre es " & Me.Nombre & " " & Me.Apellido & " y tengo " & Me.Edad & " años."
    End Function
End Class

' Definimos una clase llamada "ListaPersonas".
Public Class ListaPersonas
    ' Propiedad "Personas" de tipo List(Of Persona).
    Public Property Personas As List(Of Persona) = New List(Of Persona)

    ' Constructor de la clase "ListaPersonas".
    Public Sub New()
    End Sub

    ' Método "AñadirPersona" que añade una persona a la lista.
    Public Sub AñadirPersona(persona As Persona)
        Personas.Add(persona)
    End Sub

    ' Método "EliminarPersona" que elimina una persona de la lista.
    Public Sub EliminarPersona(persona As Persona)
        Personas.Remove(persona)
    End Sub

    ' Método "BuscarPersona" que busca una persona en la lista.
    Public Function BuscarPersona(nombre As String) As Persona
        Dim persona As Persona = Personas.Find(Function(p) p.Nombre = nombre)
        Return persona
    End Function

    ' Método "ImprimirPersonas" que imprime todas las personas de la lista.
    Public Sub ImprimirPersonas()
        For Each persona In Personas
            Console.WriteLine(persona.Saludar())
        Next
    End Sub
End Class

' Definimos una clase llamada "Programa".
Public Class Programa
    ' Punto de entrada del programa.
    Public Shared Sub Main(args As String())
        ' Creamos una lista de personas.
        Dim listaPersonas As ListaPersonas = New ListaPersonas

        ' Añadimos algunas personas a la lista.
        listaPersonas.AñadirPersona(New Persona("Juan", "García", 20))
        listaPersonas.AñadirPersona(New Persona("María", "López", 25))
        listaPersonas.AñadirPersona(New Persona("Pedro", "Sánchez", 30))

        ' Eliminamos una persona de la lista.
        listaPersonas.EliminarPersona(listaPersonas.BuscarPersona("Pedro"))

        ' Imprimimos todas las personas de la lista.
        listaPersonas.ImprimirPersonas()

        ' Esperamos a que el usuario pulse una tecla para salir del programa.
        Console.ReadKey()
    End Sub
End Class
```

Explicación del código:

* La primera parte del código define una clase llamada "Persona" con tres propiedades: "Nombre", "Apellido" y "Edad". También define un constructor para la clase que inicializa las propiedades con los valores proporcionados.
* La segunda parte del código define una clase llamada "ListaPersonas" con una propiedad llamada "Personas" que es una lista de objetos de tipo "Persona". También define un constructor para la clase que inicializa la lista vacía.
* La tercera parte del código define una clase llamada "Programa" con un método "Main" que es el punto de entrada del programa.
* El método "Main" crea una lista de personas, añade algunas personas a la lista, elimina una persona de la lista e imprime todas las personas de la lista.
* El programa espera a que el usuario pulse una tecla para salir del programa.