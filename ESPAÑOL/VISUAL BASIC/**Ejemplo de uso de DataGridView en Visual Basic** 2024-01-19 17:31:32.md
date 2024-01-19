```visual basic
Public Class Form1

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Crear un nuevo objeto DataGridView.
        Dim dgv As New DataGridView()

        ' Establecer las propiedades del DataGridView.
        dgv.Location = New Point(10, 10)
        dgv.Size = New Size(500, 300)
        dgv.ColumnHeadersVisible = True
        dgv.AllowUserToAddRows = False

        ' Agregar columnas al DataGridView.
        dgv.Columns.Add("Nombre", "Nombre")
        dgv.Columns.Add("Apellido", "Apellido")
        dgv.Columns.Add("Edad", "Edad")

        ' Agregar filas al DataGridView.
        dgv.Rows.Add("Juan", "Pérez", 20)
        dgv.Rows.Add("María", "López", 25)
        dgv.Rows.Add("Pedro", "García", 30)

        ' Agregar el DataGridView al formulario.
        Me.Controls.Add(dgv)
    End Sub

    Private Sub Form1_CellClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgv.CellClick
        ' Obtener la fila seleccionada.
        Dim row As DataGridViewRow = dgv.SelectedRows(0)

        ' Obtener los valores de las celdas de la fila seleccionada.
        Dim nombre = row.Cells("Nombre").Value
        Dim apellido = row.Cells("Apellido").Value
        Dim edad = row.Cells("Edad").Value

        ' Mostrar los valores de las celdas de la fila seleccionada en un cuadro de mensaje.
        MessageBox.Show(String.Format("Nombre: {0}\nApellido: {1}\nEdad: {2}", nombre, apellido, edad))
    End Sub
End Class
```

Explicación del código:

* La clase `Form1` es la clase principal del formulario.
* El método `Form1_Load` se ejecuta cuando se carga el formulario.
* El método `Form1_CellClick` se ejecuta cuando se hace clic en una celda del DataGridView.
* El código en el método `Form1_Load` crea un nuevo objeto DataGridView, establece sus propiedades y agrega columnas y filas al mismo.
* El código en el método `Form1_CellClick` obtiene la fila seleccionada, obtiene los valores de las celdas de la fila seleccionada y los muestra en un cuadro de mensaje.