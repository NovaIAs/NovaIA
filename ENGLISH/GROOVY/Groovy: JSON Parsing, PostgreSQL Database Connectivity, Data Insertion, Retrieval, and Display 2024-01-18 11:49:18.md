```groovy
// Import necessary libraries
import groovy.json.JsonSlurper
import groovy.sql.Sql

// Read JSON data from a remote URL
def jsonSlurper = new JsonSlurper()
def jsonObject = jsonSlurper.parse(new URL("https://example.com/data.json").openConnection())

// Connect to a SQL database
def sql = Sql.newInstance("jdbc:postgresql://localhost:5432/test", "postgres", "password")

// Iterate over the JSON data and insert it into the database
jsonObject.each { data ->
    sql.executeInsert("INSERT INTO table_name (id, name, age) VALUES (${data.id}, '${data.name}', ${data.age})")
}

// Select data from the database and display it
def results = sql.rows("SELECT * FROM table_name")
println "Results:"
results.each { data ->
    println data
}

// Close the SQL connection
sql.close()

// Cleanup
jsonSlurper = null
jsonObject = null
sql = null
results = null
```

This code demonstrates a more complex Groovy script that reads JSON data from a remote URL, connects to a PostgreSQL database, inserts the data into the database, retrieves it, displays it, and then closes the connection. It also uses Groovy's native JSON parsing and database connectivity features.