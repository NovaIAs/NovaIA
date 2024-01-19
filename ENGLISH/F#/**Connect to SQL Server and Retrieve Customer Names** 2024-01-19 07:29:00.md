```f#

// Import the F# scripting library
open FSharp.Data.SqlClient

// Define a connection string
let connectionString = "Server=.;Database=AdventureWorks;Trusted_Connection=True;"

// Create a connection to the database
let connection = new System.Data.SqlClient.SqlConnection(connectionString)

// Open the connection
connection.Open()

// Create a command to execute a query
let command = new System.Data.SqlClient.SqlCommand("SELECT * FROM Sales.SalesOrderHeader", connection)

// Execute the query and get the results
let results = command.ExecuteReader()

// Iterate over the results and print the customer names
while results.Read() do
    printfn "%s" results.Item("CustomerName")

// Close the connection
connection.Close()

```

**Explanation:**

This F# script connects to an SQL Server database, executes a query, and then prints the results.

The code first imports the F# scripting library, which provides functions for working with SQL Server.

Next, a connection string is defined. This string contains the information needed to connect to the database, such as the server name, database name, and authentication information.

A connection to the database is then created using the `System.Data.SqlClient.SqlConnection` class. The connection is opened and a command to execute a query is created using the `System.Data.SqlClient.SqlCommand` class.

The query in this example selects all columns from the Sales.SalesOrderHeader table in the AdventureWorks database.

The query is executed and the results are stored in a `System.Data.SqlClient.SqlDataReader` object. The `ExecuteReader()` method is used to execute the query and return a data reader object.

The `while` loop iterates over the results and prints the customer names. The `results.Read()` method is used to read the next row in the results. The `results.Item("CustomerName")` method is used to get the value of the CustomerName column in the current row.

Finally, the connection is closed.