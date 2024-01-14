```groovy
import groovy.json.JsonSlurper
import groovy.sql.Sql

// Define the database connection details
def dbUrl = "jdbc:postgresql://localhost:5432/groovy_db"
def dbUser = "groovy_user"
def dbPassword = "groovy_password"

// Establish a connection to the database
def sql = Sql.newInstance(dbUrl, dbUser, dbPassword, "org.postgresql.Driver")

// Define the JSON data to be inserted into the database
def jsonSlurper = new JsonSlurper()
def jsonData = jsonSlurper.parseText("""
{
  "customers": [
    {
      "id": 1,
      "name": "John Doe",
      "email": "john.doe@example.com",
      "phone": "555-123-4567"
    },
    {
      "id": 2,
      "name": "Jane Smith",
      "email": "jane.smith@example.com",
      "phone": "555-234-5678"
    },
    {
      "id": 3,
      "name": "Michael Jones",
      "email": "michael.jones@example.com",
      "phone": "555-345-6789"
    }
  ]
}
""")

// Prepare the SQL statement to insert the JSON data into the database
def insertSql = "INSERT INTO customers (id, name, email, phone) VALUES (?, ?, ?, ?)"

// Iterate over the JSON data and insert each customer record into the database
jsonData.customers.each { customer ->
  sql.executeInsert(insertSql, [customer.id, customer.name, customer.email, customer.phone])
}

// Commit the changes to the database
sql.commit()

// Close the database connection
sql.close()

// Print a success message
println "Customers data successfully inserted into the database."
```

This Groovy code performs the following tasks:

1. It imports the necessary Groovy libraries, including the `JsonSlurper` for parsing JSON data and the `Sql` class for interacting with the database.

2. It defines the database connection details, including the URL, username, password, and driver class.

3. It establishes a connection to the PostgreSQL database using the provided connection details.

4. It defines the JSON data to be inserted into the database using the `JsonSlurper` to parse the JSON text.

5. It prepares an SQL statement to insert the JSON data into the `customers` table, specifying the columns to be inserted.

6. It iterates over the JSON data and inserts each customer record into the database using the `executeInsert` method of the `Sql` object.

7. It commits the changes to the database to make them permanent.

8. It closes the database connection to release the resources.

9. Finally, it prints a success message indicating that the customers data was successfully inserted into the database.