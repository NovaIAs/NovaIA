```groovy
import groovy.json.JsonSlurper
import groovy.sql.Sql

// Parse a JSON file into a Groovy map
def jsonSlurper = new JsonSlurper()
def jsonObject = jsonSlurper.parse(new File('data.json'))

// Connect to a database
def sql = Sql.newInstance('jdbc:postgresql://localhost:5432/mydb', 'postgres', 'mypassword')

// Create a table if it doesn't exist
sql.execute('CREATE TABLE IF NOT EXISTS users (id INT PRIMARY KEY, name TEXT, email TEXT)')

// Insert data from the JSON object into the database
jsonObject.each { user ->
  sql.execute("INSERT INTO users (id, name, email) VALUES (${user.id}, '${user.name}', '${user.email}')")
}

// Query the database and print the results
sql.eachRow('SELECT * FROM users') { row ->
  println "User ${row.id}: ${row.name} (${row.email})"
}

// Close the database connection
sql.close()
```

This code performs the following tasks:

1. Parses a JSON file into a Groovy map using the `JsonSlurper`.
2. Connects to a PostgreSQL database using the `Sql` class.
3. Creates a table named `users` if it doesn't exist.
4. Inserts data from the JSON object into the `users` table using a loop.
5. Queries the `users` table and prints the results.
6. Closes the database connection.

This code demonstrates several Groovy features, including:

* Using the `JsonSlurper` to parse JSON data.
* Using the `Sql` class to connect to a database and execute SQL queries.
* Using a loop to iterate over a collection.
* Using string interpolation to embed variables in strings.
* Using the `println()` function to print output to the console.