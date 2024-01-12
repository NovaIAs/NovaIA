```groovy
import groovy.json.JsonOutput
import groovy.json.JsonBuilder

// Create a JSON object representing a person
def person = new JsonBuilder()
person {
    name "John Doe"
    age 30
    address {
        street "123 Main Street"
        city "Anytown"
        state "CA"
        zip "12345"
    }
    phone_numbers [
        "1-800-555-1212",
        "1-800-555-1213"
    ]
}

// Convert the JSON object to a string
def json = JsonOutput.toJson(person)

// Print the JSON string to the console
println json

// Create a JSON array representing a list of people
def people = [
    [
        name: "John Doe",
        age: 30,
        address: [
            street: "123 Main Street",
            city: "Anytown",
            state: "CA",
            zip: "12345"
        ],
        phone_numbers: [
            "1-800-555-1212",
            "1-800-555-1213"
        ]
    ],
    [
        name: "Mary Jane",
        age: 25,
        address: [
            street: "456 Elm Street",
            city: "Anytown",
            state: "CA",
            zip: "12345"
        ],
        phone_numbers: [
            "1-800-555-1214",
            "1-800-555-1215"
        ]
    ]
]

// Convert the JSON array to a string
json = JsonOutput.toJson(people)

// Print the JSON string to the console
println json

// Create a JSON object representing a complex data structure
def data = new JsonBuilder()
data {
    people people
    addresses [
        [
            street: "123 Main Street",
            city: "Anytown",
            state: "CA",
            zip: "12345"
        ],
        [
            street: "456 Elm Street",
            city: "Anytown",
            state: "CA",
            zip: "12345"
        ]
    ]
    phone_numbers [
        "1-800-555-1212",
        "1-800-555-1213",
        "1-800-555-1214",
        "1-800-555-1215"
    ]
}

// Convert the JSON object to a string
json = JsonOutput.toJson(data)

// Print the JSON string to the console
println json
```

This code creates JSON data structures representing a person, a list of people, and a complex data structure with multiple levels of nesting. The data is then converted to JSON strings and printed to the console.

The `JsonBuilder` class is used to create JSON data structures in a concise and easy-to-read manner. The `JsonOutput` class is used to convert JSON data structures to strings.

The resulting JSON strings can be used for a variety of purposes, such as sending data to a web service or persisting data to a database.