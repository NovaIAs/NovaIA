```visual basic
' Este código crea una interfaz gráfica de usuario (GUI) simple en Visual Basic.

' Primero, importamos la biblioteca de formularios de Windows.
Imports System.Windows.Forms

' A continuación, creamos una nueva clase llamada "VentanaPrincipal" que hereda de la
' clase "Form" de la biblioteca de formularios de Windows.
Public Class VentanaPrincipal
    Inherits Form

    ' Declaramos los elementos de la GUI que vamos a utilizar.
    Private lblTexto As New Label()
    Private txtEntrada As New TextBox()
    Private btnAceptar As New Button()

    ' Creamos el constructor de la clase "VentanaPrincipal".
    Public Sub New()
        ' Inicializamos el formulario y sus elementos.
        Me.Text = "Ventana principal"
        Me.Size = New Size(300, 200)
        Me.StartPosition = FormStartPosition.CenterScreen

        lblTexto.Text = "Introduce un texto:"
        lblTexto.Location = New Point(10, 10)
        Me.Controls.Add(lblTexto)

        txtEntrada.Location = New Point(10, 30)
        txtEntrada.Size = New Size(200, 20)
        Me.Controls.Add(txtEntrada)

        btnAceptar.Text = "Aceptar"
        btnAceptar.Location = New Point(10, 60)
        btnAceptar.Click += New EventHandler(AddressOf btnAceptar_Click)
        Me.Controls.Add(btnAceptar)
    End Sub

    ' Creamos el evento "Click" del botón "btnAceptar".
    Private Sub btnAceptar_Click(sender As Object, e As EventArgs)
        ' Obtenemos el texto introducido en el cuadro de texto "txtEntrada".
        Dim textoIntroducido As String = txtEntrada.Text

        ' Mostramos el texto introducido en un cuadro de mensaje.
        MessageBox.Show("El texto introducido es: " & textoIntroducido)
    End Sub
End Class

' Creamos una nueva instancia de la clase "VentanaPrincipal" y la mostramos.
Dim ventanaPrincipal As New VentanaPrincipal()
ventanaPrincipal.ShowDialog()
```

Explicación del código:

* La primera línea importa la biblioteca de formularios de Windows, que contiene las clases y los controles necesarios para crear una GUI.
* La clase "VentanaPrincipal" hereda de la clase "Form" de la biblioteca de formularios de Windows, lo que nos permite crear un formulario nuevo.
* Dentro de la clase "VentanaPrincipal", declaramos los elementos de la GUI que vamos a utilizar: una etiqueta ("lblTexto"), un cuadro de texto ("txtEntrada") y un botón ("btnAceptar").
* En el constructor de la clase "VentanaPrincipal", inicializamos el formulario y sus elementos. Establecemos el texto del formulario, su tamaño y su posición en la pantalla. También añadimos los controles a la colección de controles del formulario.
* Creamos el evento "Click" del botón "btnAceptar". Cuando el usuario hace clic en el botón, obtenemos el texto introducido en el cuadro de texto "txtEntrada" y lo mostramos en un cuadro de mensaje.
* Finalmente, creamos una nueva instancia de la clase "VentanaPrincipal" y la mostramos.

Este código crea una GUI simple que permite al usuario introducir un texto y mostrarlo en un cuadro de mensaje.