```groovy
// Import necessary libraries
import groovy.sql.Sql
import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

// Define a class to represent a 'User' object
@CompileStatic
class User {
    // Properties of the 'User' class
    Long id
    String name
    String email
    String password

    // Constructor to initialize the 'User' object
    User(Long id, String name, String email, String password) {
        this.id = id
        this.name = name
        this.email = email
        this.password = password
    }

    // Method to convert the 'User' object to a JSON representation
    String toJson() {
        // Create a JSON builder object
        JsonBuilder jsonBuilder = new JsonBuilder()

        // Add the properties of the 'User' object to the JSON builder
        jsonBuilder {
            id id
            name name
            email email
            password password
        }

        // Convert the JSON builder object to a JSON string and return it
        return jsonBuilder.toPrettyString()
    }
}

// Define a class to represent a 'Database' object
@CompileStatic
class Database {
    // Static field to store the database connection
    private static Sql sql

    // Static method to initialize the database connection
    static void init() {
        // Create a new SQL connection object
        sql = Sql.newInstance("jdbc:h2:mem:test", "sa", "", "org.h2.Driver")

        // Create a 'users' table if it doesn't exist
        sql.execute("""
            CREATE TABLE IF NOT EXISTS users (
                id BIGINT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(255) NOT NULL,
                email VARCHAR(255) NOT NULL UNIQUE,
                password VARCHAR(255) NOT NULL
            )
        """)
    }

    // Static method to insert a new 'User' object into the database
    static void insertUser(User user) {
        // Prepare the SQL statement to insert the 'User' object
        String sqlStatement = "INSERT INTO users (name, email, password) VALUES (?, ?, ?)"

        // Execute the SQL statement with the 'User' object's properties as parameters
        sql.execute(sqlStatement, [user.name, user.email, user.password])
    }

    // Static method to select all 'User' objects from the database
    static List<User> getAllUsers() {
        // Prepare the SQL statement to select all 'User' objects
        String sqlStatement = "SELECT * FROM users"

        // Execute the SQL statement and return the results as a list of 'User' objects
        return sql.rows(sqlStatement).collect { row -> new User(row.id, row.name, row.email, row.password) }
    }

    // Static method to select a 'User' object by its id
    static User getUserById(Long id) {
        // Prepare the SQL statement to select a 'User' object by its id
        String sqlStatement = "SELECT * FROM users WHERE id = ?"

        // Execute the SQL statement with the id as a parameter and return the first row as a 'User' object
        return sql.firstRow(sqlStatement, [id]) { row -> new User(row.id, row.name, row.email, row.password) }
    }

    // Static method to update a 'User' object in the database
    static void updateUser(User user) {
        // Prepare the SQL statement to update the 'User' object
        String sqlStatement = "UPDATE users SET name = ?, email = ?, password = ? WHERE id = ?"

        // Execute the SQL statement with the 'User' object's properties as parameters
        sql.execute(sqlStatement, [user.name, user.email, user.password, user.id])
    }

    // Static method to delete a 'User' object from the database
    static void deleteUser(Long id) {
        // Prepare the SQL statement to delete the 'User' object
        String sqlStatement = "DELETE FROM users WHERE id = ?"

        // Execute the SQL statement with the id as a parameter
        sql.execute(sqlStatement, [id])
    }
}

// Initialize the database connection
Database.init()

// Create a new 'User' object
User user = new User(null, "John Doe", "johndoe@example.com", "password")

// Insert the 'User' object into the database
Database.insertUser(user)

// Select all 'User' objects from the database and print them as JSON strings
Database.getAllUsers().each { println it.toJson() }

// Select the 'User' object with id 1 from the database and print it as a JSON string
println Database.getUserById(1).toJson()

// Update the 'User' object with id 1
user.name = "Jane Doe"
Database.updateUser(user)

// Delete the 'User' object with id 1 from the database
Database.deleteUser(1)
```

**Explanation:**

This Groovy script demonstrates various complex and differentiated features, including:

1. **Database Connection and CRUD Operations:** It uses the Groovy SQL library to connect to a H2 in-memory database and perform CRUD (Create, Read, Update, Delete) operations on a 'User' table.

2. **Custom 'User' Class:** It defines a 'User' class to represent a user with properties like id, name, email, and password. The class includes methods to convert the 'User' object to a JSON representation.

3. **Database Initialization:** It initializes the database connection and creates the 'users' table if it doesn't exist.

4. **Inserting a New User:** It demonstrates how to insert a new 'User' object into the database using a prepared SQL statement.

5. **Retrieving All Users:** It shows how to select all 'User' objects from the database and convert them into a list of 'User' objects.

6. **Retrieving a User by ID:** It demonstrates how to select a specific 'User' object from the database by its id.

7. **Updating a User:** It shows how to update an existing 'User' object in the database using a prepared SQL statement.

8. **Deleting a User:** It demonstrates how to delete a 'User' object from the database by its id.

9. **JSON Conversion:** It uses the Groovy JSON library to convert 'User' objects to JSON representations, which can be useful for data exchange or API responses.

10. **Multiple Database Operations:** It demonstrates how to perform multiple database operations, such as inserting, retrieving, updating, and deleting data, in a single script.

This code showcases advanced Groovy features and demonstrates a practical example of working with a database and manipulating data in a complex and differentiated manner.