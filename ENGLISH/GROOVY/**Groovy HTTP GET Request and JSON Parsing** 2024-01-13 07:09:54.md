```groovy
import groovy.json.JsonSlurper

// Define the URL of the API endpoint
def apiUrl = 'https://example.com/api/v1/data'

// Create a new HTTP client
def client = new HTTPBuilder(apiUrl)

// Prepare the HTTP request
def request = client.get(contentType: 'application/json')

// Send the request and get the response
def response = request.execute()

// Parse the JSON response
def jsonSlurper = new JsonSlurper()
def jsonData = jsonSlurper.parseText(response.content.text)

// Extract the data from the JSON response
def data = jsonData.data

// Iterate over the data and print each item
data.each { item ->
    println item.name + ": " + item.value
}
```

Explanation:

* The code starts by importing the `groovy.json.JsonSlurper` class, which is used to parse JSON data.
* The URL of the API endpoint is defined as a string variable.
* A new HTTP client is created using the `HTTPBuilder` class.
* An HTTP GET request is prepared and the content type is set to `application/json`.
* The request is sent and the response is received.
* The JSON response is parsed using the `JsonSlurper` class.
* The data from the JSON response is extracted and stored in a variable.
* The data is iterated over and each item is printed to the console using the `println` method.

This code demonstrates how to make an HTTP GET request to an API endpoint, parse the JSON response, and extract the data from the response. It is a more complex example of using Groovy to interact with an API.