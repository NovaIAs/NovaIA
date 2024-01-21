```visual basic
' Módulo principal del código
Public Sub Main()
    ' Crear una instancia de la clase FormularioPrincipal
    Dim formularioPrincipal As New FormularioPrincipal

    ' Mostrar el formulario principal
    formularioPrincipal.Show()
End Sub

' Clase que representa el formulario principal de la aplicación
Public Class FormularioPrincipal

    ' Constructor de la clase
    Public Sub New()
        ' Establecer las propiedades del formulario
        Me.Text = "Formulario Principal"
        Me.Width = 600
        Me.Height = 400

        ' Crear una instancia de la clase Botón
        Dim boton1 As New Botón

        ' Establecer las propiedades del botón
        boton1.Text = "Botón 1"
        boton1.Location = New Point(100, 100)

        ' Agregar el botón al formulario
        Me.Controls.Add(boton1)
    End Sub

    ' Evento que se produce cuando se hace clic en el botón
    Private Sub Boton1_Click(sender As Object, e As EventArgs)
        ' Mostrar un mensaje en una ventana emergente
        MessageBox.Show("¡Hola, mundo!", "Mensaje del botón 1")
    End Sub
End Class

' Clase que representa un botón
Public Class Botón

    ' Constructor de la clase
    Public Sub New()
        ' Establecer las propiedades del botón
        Me.Text = "Botón"
        Me.Width = 100
        Me.Height = 25
    End Sub

    ' Evento que se produce cuando se hace clic en el botón
    Private Sub Boton_Click(sender As Object, e As EventArgs)
        ' Mostrar un mensaje en una ventana emergente
        MessageBox.Show("¡Hola, mundo!", "Mensaje del botón")
    End Sub
End Class
```

Explicación del código:

* El código crea una instancia de la clase `FormularioPrincipal` y la muestra en la pantalla.
* El formulario principal contiene un botón con el texto "Botón 1".
* Cuando se hace clic en el botón, se muestra un mensaje en una ventana emergente.
* La clase `Botón` define el comportamiento del botón, incluyendo el evento `Click` que se produce cuando se hace clic en el botón.
* El código también crea una instancia de la clase `Botón` y la agrega al formulario principal.
* Cuando se hace clic en el botón, se muestra un mensaje en una ventana emergente.