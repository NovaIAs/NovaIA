```visual basic

' Definir una clase personalizada para crear objetos de tipo "Persona"
Class Persona
    ' Propiedades de la clase
    Private nombre As String
    Private edad As Integer
    Private direccion As String

    ' Método constructor de la clase
    Public Sub New(nombre As String, edad As Integer, direccion As String)
        Me.nombre = nombre
        Me.edad = edad
        Me.direccion = direccion
    End Sub

    ' Métodos de la clase
    Public Function ObtenerNombre() As String
        Return Me.nombre
    End Function

    Public Function ObtenerEdad() As Integer
        Return Me.edad
    End Function

    Public Function ObtenerDireccion() As String
        Return Me.direccion
    End Function

    Public Sub EstablecerNombre(nombre As String)
        Me.nombre = nombre
    End Sub

    Public Sub EstablecerEdad(edad As Integer)
        Me.edad = edad
    End Sub

    Public Sub EstablecerDireccion(direccion As String)
        Me.direccion = direccion
    End Sub
End Class

' Crear un procedimiento para mostrar un mensaje de bienvenida a una persona
Public Sub MostrarBienvenida(persona As Persona)
    Dim mensaje As String = "Bienvenido, " & persona.ObtenerNombre() & "!"
    MsgBox mensaje
End Sub

' Crear un procedimiento para mostrar la información de una persona
Public Sub MostrarInformacionPersona(persona As Persona)
    Dim mensaje As String = "Nombre: " & persona.ObtenerNombre() & vbCrLf _
        & "Edad: " & persona.ObtenerEdad() & vbCrLf _
        & "Dirección: " & persona.ObtenerDireccion()

    MsgBox mensaje
End Sub

' Crear un procedimiento para crear una nueva persona y mostrar su información
Public Sub CrearPersona()
    ' Crear una nueva instancia de la clase "Persona"
    Dim persona1 As Persona = New Persona("Juan", 25, "Calle 123")

    ' Mostrar la información de la persona creada
    MostrarInformacionPersona persona1
End Sub

' Crear un procedimiento para modificar la información de una persona y mostrar su información actualizada
Public Sub ModificarPersona()
    ' Crear una nueva instancia de la clase "Persona"
    Dim persona1 As Persona = New Persona("Juan", 25, "Calle 123")

    ' Modificar la información de la persona creada
    persona1.EstablecerNombre("María")
    persona1.EstablecerEdad(30)
    persona1.EstablecerDireccion("Calle 321")

    ' Mostrar la información de la persona actualizada
    MostrarInformacionPersona persona1
End Sub


```

Este código crea una clase personalizada llamada "Persona", que tiene tres propiedades: "nombre", "edad" y "direccion". También incluye métodos para obtener y establecer el valor de cada propiedad.

A continuación, se definen tres procedimientos:

* **MostrarBienvenida:** Este procedimiento muestra un mensaje de bienvenida a una persona.

* **MostrarInformacionPersona:** Este procedimiento muestra la información de una persona en un cuadro de mensaje.

* **CrearPersona:** Este procedimiento crea una nueva instancia de la clase "Persona" y muestra su información.

* **ModificarPersona:** Este procedimiento modifica la información de una persona y muestra su información actualizada.

El código se ejecuta de la siguiente manera:

1. Se crea una nueva instancia de la clase "Persona" llamada "persona1" y se le pasan los valores iniciales de las propiedades "nombre", "edad" y "direccion".

2. Se llama al procedimiento "MostrarInformacionPersona" para mostrar la información de la persona creada.

3. Se llama al procedimiento "ModificarPersona" para modificar la información de la persona creada.

4. Se vuelve a llamar al procedimiento "MostrarInformacionPersona" para mostrar la información actualizada de la persona.