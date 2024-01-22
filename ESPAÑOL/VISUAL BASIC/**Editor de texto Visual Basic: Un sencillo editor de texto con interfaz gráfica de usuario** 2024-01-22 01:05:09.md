```visual basic

' Definir constantes para la interfaz de usuario.
Const ID_BOTÓN_NUEVO = 100
Const ID_BOTÓN_ABRIR = 101
Const ID_BOTÓN_GUARDAR = 102
Const ID_BOTÓN_SALIR = 103
Const ID_EDIT_TEXTO = 104

' Definir la ventana principal.
Dim frmPrincipal As New Form
frmPrincipal.Text = "Editor de texto"
frmPrincipal.Width = 600
frmPrincipal.Height = 400

' Crear la barra de herramientas.
Dim tlbBarraHerramientas As New ToolBar
tlbBarraHerramientas.Parent = frmPrincipal
tlbBarraHerramientas.Dock = DockStyle.Top
tlbBarraHerramientas.Buttons.Add(New ToolBarButton(ID_BOTÓN_NUEVO, "Nuevo"))
tlbBarraHerramientas.Buttons.Add(New ToolBarButton(ID_BOTÓN_ABRIR, "Abrir"))
tlbBarraHerramientas.Buttons.Add(New ToolBarButton(ID_BOTÓN_GUARDAR, "Guardar"))
tlbBarraHerramientas.Buttons.Add(New ToolBarButton(ID_BOTÓN_SALIR, "Salir"))

' Crear el área de texto.
Dim txtTexto As New TextBox
txtTexto.Parent = frmPrincipal
txtTexto.Dock = DockStyle.Fill
txtTexto.Multiline = True
txtTexto.WordWrap = True

' Agregar eventos a la barra de herramientas.
AddHandler tlbBarraHerramientas.ButtonClick, AddressOf BarraHerramientas_ButtonClick

' Mostrar la ventana principal.
frmPrincipal.Show()

' Función para manejar los eventos de la barra de herramientas.
Private Sub BarraHerramientas_ButtonClick(sender As Object, e As ToolBarButtonClickEventArgs)
    Select Case e.Button.ID
        Case ID_BOTÓN_NUEVO
            txtTexto.Text = ""
        Case ID_BOTÓN_ABRIR
            Dim dlgAbrir As New OpenFileDialog
            If dlgAbrir.ShowDialog() = DialogResult.OK Then
                txtTexto.Text = File.ReadAllText(dlgAbrir.FileName)
            End If
        Case ID_BOTÓN_GUARDAR
            Dim dlgGuardar As New SaveFileDialog
            If dlgGuardar.ShowDialog() = DialogResult.OK Then
                File.WriteAllText(dlgGuardar.FileName, txtTexto.Text)
            End If
        Case ID_BOTÓN_SALIR
            End
    End Select
End Sub

```

Explicación del código:

* El código comienza definiendo constantes para los identificadores de los controles de la interfaz de usuario, como los botones de la barra de herramientas y el área de texto.
* A continuación, se crea la ventana principal y se configura su título, tamaño y posición.
* Luego, se crea la barra de herramientas y se añaden los botones correspondientes a las acciones de nuevo, abrir, guardar y salir.
* También se crea el área de texto, que es donde el usuario escribirá el texto.
* Posteriormente, se añaden eventos a los botones de la barra de herramientas para que ejecuten las acciones correspondientes cuando sean pulsados.
* Finalmente, se muestra la ventana principal en pantalla.

El código está bien organizado y comentado, lo que facilita su lectura y comprensión. Además, es extensible, ya que se pueden añadir fácilmente nuevas características o modificar las existentes.