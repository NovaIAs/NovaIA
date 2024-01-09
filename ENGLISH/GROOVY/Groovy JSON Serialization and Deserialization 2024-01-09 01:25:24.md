```groovy
import groovy.json.JsonSlurper

// Define a complex data structure
def complexData = [
    name: "John Doe",
    age: 30,
    address: [
        street: "123 Main Street",
        city: "Anytown",
        state: "CA",
        zip: "12345"
    ],
    phoneNumbers: [
        "home": "1-800-555-1212",
        "work": "1-800-555-1213",
        "mobile": "1-800-555-1214"
    ],
    interests: ["programming", "music", "travel"],
    friends: [
        ["name": "Jane Smith", "age": 25],
        ["name": "Bob Jones", "age": 35]
    ]
]

// Convert the data structure to JSON using Groovy's built-in JSON library
def json = JsonSlurper.serialize(complexData)

// Print the JSON string to the console
println json

// Parse the JSON string back into a data structure using Groovy's built-in JSON library
def parsedData = new JsonSlurper().parseText(json)

// Print the parsed data structure to the console
println parsedData
```

This code demonstrates a number of Groovy features, including:

* The use of a complex data structure, including nested maps and lists.
* The use of Groovy's built-in JSON library to convert the data structure to and from JSON.
* The use of Groovy's concise syntax, such as the `println` statement and the `new JsonSlurper().parseText(json)` expression.