```groovy
import groovy.json.JsonSlurper

// Define the URL of the API endpoint
def apiUrl = "https://api.example.com/v1/users"

// Create a JSON slurper to parse the JSON response from the API
def jsonSlurper = new JsonSlurper()

// Make a GET request to the API endpoint and store the response in a variable
def response = jsonSlurper.parse(new URL(apiUrl).openConnection().getInputStream())

// Iterate over the list of users in the JSON response
response.each { user ->
  // Set desired font styles for each user record
  println("Formatting user ${user.id}:")
  println("-------------------")
  println("Name:")
  setBold()
  println("| ${user.name}")
  setItalic()
  println("| ${user.email}")
  println() // Add an empty line for readability

  // List the user's roles
  println("Roles:")
  user.roles.each { role ->
    setUnderline()
    println("| ${role}")
  }

  println() // Add an empty line for readability
  println() // Add an empty line for readability
}

// Define a method to set the font to bold
def setBold() {
  println("```bold")
}

// Define a method to set the font to italic
def setItalic() {
  println("```italic")
}

// Define a method to set the font to underline
def setUnderline() {
  println("```underline")
}
```
Explanation:

1. Import the Groovy JSON library: `import groovy.json.JsonSlurper`.

2. Define the API endpoint URL: `def apiUrl = "https://api.example.com/v1/users"`.

3. Create a JSON slurper to parse the JSON response: `def jsonSlurper = new JsonSlurper()`.

4. Make a GET request to the API endpoint using `new URL(apiUrl).openConnection().getInputStream()` and parse the JSON response using `jsonSlurper.parse()`. Store the result in the `response` variable.

5. Iterate through the `response` JSON object, which contains a list of user objects.

6. For each user, print their ID, name, email, and roles.

7. Use the `setBold()`, `setItalic()`, and `setUnderline()` methods to change the font style for specific parts of the output.

8. Define the `setBold()`, `setItalic()`, and `setUnderline()` methods to set the font styles using ANSI escape codes (````bold`, ````italic`, and ````underline`).

This code demonstrates how to make a GET request to an API endpoint, parse the JSON response, iterate through the JSON data, and format the output using different font styles.