```visual basic

' Crear una base de datos
Dim oDB As New DAO.Database
oDB.Create "C:\MiBaseDeDatos.mdb"

' Crear una tabla
Dim oTabla As New DAO.TableDef
oTabla.Name = "Productos"
oTabla.Fields.Append "ID", DAO.Field.Long, DAO.FieldSizeByte
oTabla.Fields.Append "Nombre", DAO.Field.Text, 50
oTabla.Fields.Append "Precio", DAO.Field.Currency
oDB.Tables.Append oTabla

' Insertar datos en la tabla
Dim oRegistro As New DAO.Recordset
oRegistro.Open "Productos", oDB, DAO.dbOpenDynaset

oRegistro.AddNew
oRegistro("Nombre") = "Manzana"
oRegistro("Precio") = 1.5
oRegistro.Update

oRegistro.AddNew
oRegistro("Nombre") = "Naranja"
oRegistro("Precio") = 2.0
oRegistro.Update

oRegistro.AddNew
oRegistro("Nombre") = "Plátano"
oRegistro("Precio") = 3.0
oRegistro.Update

oRegistro.Close

' Crear un formulario
Dim oFormulario As New Form
oFormulario.Name = "FormularioProductos"
oFormulario.Caption = "Productos"

' Agregar un control ListBox al formulario
Dim oListBox As New ListBox
oListBox.Name = "ListBoxProductos"
oListBox.Top = 10
oListBox.Left = 10
oListBox.Width = 300
oListBox.Height = 200
oFormulario.Controls.Add oListBox

' Agregar un control TextBox al formulario
Dim oTextBox As New TextBox
oTextBox.Name = "TextBoxPrecio"
oTextBox.Top = 220
oTextBox.Left = 10
oTextBox.Width = 100
oTextBox.Height = 20
oTextBox.ReadOnly = True
oFormulario.Controls.Add oTextBox

' Agregar un control Button al formulario
Dim oBoton As New CommandButton
oBoton.Name = "BotonMostrarPrecio"
oBoton.Caption = "Mostrar Precio"
oBoton.Top = 250
oBoton.Left = 10
oBoton.Width = 100
oBoton.Height = 20
oFormulario.Controls.Add oBoton

' Agregar código al evento Click del botón
oBoton.Click += Sub(sender As Object, e As EventArgs)
    Dim sProductoSelecionado As String = oListBox.SelectedItem
    Dim oPrecio As Variant = oDB.ExecuteScalar("SELECT Precio FROM Productos WHERE Nombre='" & sProductoSelecionado & "'")
    If Not IsNull(oPrecio) Then oTextBox.Text = Format(oPrecio, "Currency")
End Sub

' Mostrar el formulario
oFormulario.Show

' Cerrar la base de datos
oDB.Close

```

Explicación del código:

1. Se crea una base de datos en la ruta "C:\MiBaseDeDatos.mdb".


2. Se crea una tabla llamada "Productos" con tres campos: "ID", "Nombre" y "Precio".


3. Se insertan tres registros en la tabla "Productos".


4. Se crea un formulario llamado "FormularioProductos" con un control ListBox, un control TextBox y un control Button.


5. Se agrega código al evento Click del botón para mostrar el precio del producto seleccionado en el ListBox.


6. Se muestra el formulario "FormularioProductos".


7. Se cierra la base de datos.