```groovy
import groovy.json.JsonSlurper
import groovy.sql.Sql
import groovy.transform.CompileStatic

@CompileStatic
class ComplexGroovyCode {

    static void main(String[] args) {
        // Initialize a JSON slurper to parse JSON data
        def jsonSlurper = new JsonSlurper()

        // Read JSON data from a file and convert it into a Map
        def jsonMap = jsonSlurper.parseFile("data.json")

        // Initialize a SQL connection to a database
        def sql = Sql.newInstance("jdbc:mysql://localhost:3306/mydb", "username", "password")

        // Iterate over the JSON map and insert data into the database
        jsonMap.each { key, value ->
            sql.execute("INSERT INTO my_table (id, name, value) VALUES (${key}, '${value.name}', ${value.value})")
        }

        // Close the SQL connection
        sql.close()

        // Print a success message
        println("Data successfully inserted into the database.")
    }
}
```

Explanation:

1. **JSON Parsing**:
   - We use the `JsonSlurper` class to read and parse JSON data from a file.
   - We convert the parsed JSON data into a Map, where the keys are the JSON property names, and the values are the corresponding values.

2. **Database Connection**:
   - We establish a connection to the MySQL database using the `Sql` class.
   - We provide the JDBC URL, username, and password as parameters.

3. **Data Insertion**:
   - We iterate over the JSON map using the `each` method.
   - For each key-value pair, we construct an SQL INSERT statement using a Groovy string template.
   - We execute the INSERT statement using the `execute` method of the `Sql` object.

4. **Closing the Connection**:
   - After all the data is inserted, we close the SQL connection to release resources.

5. **Success Message**:
   - Finally, we print a success message to indicate that the data was successfully inserted into the database.