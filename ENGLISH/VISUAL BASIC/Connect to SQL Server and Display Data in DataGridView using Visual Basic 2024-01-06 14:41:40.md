```visual basic
' Import necessary libraries
Imports System.Data.SqlClient
Imports System.Data

' Define the connection string to the database
Dim connectionString As String = "Data Source=.\SQLEXPRESS;Initial Catalog=AdventureWorks2012;Integrated Security=True"

' Create a new SQL connection
Using connection As New SqlConnection(connectionString)
    ' Open the connection
    connection.Open()

    ' Create a new SQL command to retrieve data from the database
    Dim command As New SqlCommand("SELECT * FROM Sales.SalesOrderHeader", connection)

    ' Execute the command and store the results in a data reader
    Using reader As SqlDataReader = command.ExecuteReader()
        ' Create a new DataTable to store the data
        Dim table As New DataTable()

        ' Load the data from the data reader into the DataTable
        table.Load(reader)

        ' Display the data in a DataGridView
        DataGridView1.DataSource = table
    End Using
End Using
```

This code connects to a SQL Server database, executes a query to retrieve data from the database, and then displays the data in a DataGridView control. The code is written in Visual Basic and uses the System.Data.SqlClient and System.Data namespaces to interact with the database.

The code begins by importing the necessary libraries. The System.Data.SqlClient library provides classes for connecting to and interacting with a SQL Server database, while the System.Data library provides classes for working with data in a variety of formats.

Next, the connection string is defined. The connection string specifies the server name, database name, and authentication method for the database connection. In this case, the connection string is set to use integrated security, which means that the current Windows user will be used to authenticate to the database.

A new SQL connection is then created using the connection string. The connection is then opened, which allows the code to interact with the database.

A new SQL command is then created to retrieve data from the database. The command is set to execute the query "SELECT * FROM Sales.SalesOrderHeader", which will retrieve all of the data from the SalesOrderHeader table in the Sales database.

The command is then executed and the results are stored in a data reader. A data reader is a forward-only, read-only stream of data that can be used to iterate through the results of a query.

A new DataTable is then created to store the data from the data reader. The data reader is then used to load the data into the DataTable.

Finally, the data from the DataTable is displayed in a DataGridView control. The DataGridView control is a Windows Forms control that allows users to view and edit data in a tabular format.

This code is a complex example of how to connect to a SQL Server database, execute a query, and display the results in a DataGridView control. The code can be modified to connect to a different database, execute a different query, or display the results in a different format.