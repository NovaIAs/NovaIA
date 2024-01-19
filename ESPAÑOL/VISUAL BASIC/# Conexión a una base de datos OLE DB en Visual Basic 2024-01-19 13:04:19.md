```visual basic
' Librerías necesarias.
Imports System.Data.OleDb
Imports System.Windows.Forms

' Clase principal.
Public Class Form1

    ' Declaración de variables.
    Dim connectionString As String
    Dim connection As OleDbConnection
    Dim command As OleDbCommand
    Dim adapter As OleDbDataAdapter
    Dim dataTable As DataTable

    ' Método principal.
    Public Sub Main()

        ' Inicialización de variables.
        connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Users\Usuario\Desktop\Ejemplo.mdb"
        connection = New OleDbConnection(connectionString)
        command = New OleDbCommand()
        adapter = New OleDbDataAdapter()
        dataTable = New DataTable()

        ' Apertura de la conexión.
        connection.Open()

        ' Creación del comando.
        command.Connection = connection
        command.CommandText = "SELECT * FROM TablaEjemplo"

        ' Ejecución del comando.
        adapter.SelectCommand = command
        adapter.Fill(dataTable)

        ' Cierre de la conexión.
        connection.Close()

        ' Recorrido de la tabla.
        For Each row As DataRow In dataTable.Rows
            Console.WriteLine(row("Campo1"))
            Console.WriteLine(row("Campo2"))
            Console.WriteLine(row("Campo3"))
        Next

    End Sub

End Class
```

Explicación del código:

* La librería `System.Data.OleDb` es necesaria para trabajar con bases de datos OLE DB.
* La librería `System.Windows.Forms` es necesaria para crear la interfaz gráfica de usuario.
* La clase `Form1` es la clase principal de la aplicación.
* La variable `connectionString` contiene la cadena de conexión a la base de datos.
* La variable `connection` es la conexión a la base de datos.
* La variable `command` es el comando que se ejecuta contra la base de datos.
* La variable `adapter` es el adaptador de datos que se utiliza para rellenar el conjunto de datos.
* La variable `dataTable` es el conjunto de datos que contiene los datos de la tabla.
* El método `Main` es el método principal de la aplicación.
* El método `Open` abre la conexión a la base de datos.
* El método `CommandText` establece el comando que se ejecuta contra la base de datos.
* El método `SelectCommand` establece el comando que se utiliza para rellenar el conjunto de datos.
* El método `Fill` rellena el conjunto de datos con los datos de la tabla.
* El método `Close` cierra la conexión a la base de datos.
* El bucle `For Each` recorre las filas del conjunto de datos.
* El método `WriteLine` escribe una línea de texto en la consola.