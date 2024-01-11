```kotlin
// Create a sealed class to represent the different types of network responses
sealed class NetworkResponse<out T> {
    // Represent a successful response with a body of type T
    data class Success<T>(val body: T) : NetworkResponse<T>()

    // Represent an error response with an error code and a message
    data class Error(val code: Int, val message: String) : NetworkResponse<Nothing>()

    // Represent a loading state
    object Loading : NetworkResponse<Nothing>()
}

// Create a suspend function to fetch data from a remote API
suspend fun <T> fetchApiData(url: String): NetworkResponse<T> {
    // Simulate a network request by suspending the coroutine for a short time
    delay(1000)

    // Randomly generate a response
    val random = Random()
    return when (random.nextInt(3)) {
        0 -> NetworkResponse.Success(generateRandomData())
        1 -> NetworkResponse.Error(404, "Not Found")
        else -> NetworkResponse.Loading
    }
}

// Generate random data of type T
private fun <T> generateRandomData(): T {
    // Here, we can use a library like Faker to generate random data
    // For simplicity, we just return a random integer
    return Random().nextInt()
}

// Create a coroutine scope to launch a new coroutine
val scope = CoroutineScope(Dispatchers.IO)

// Launch a new coroutine to fetch data from the API
scope.launch {
    // Handle the result of the API call
    when (val response = fetchApiData<Int>("https://example.com/api")) {
        is NetworkResponse.Success -> {
            // The request was successful, process the data
            println("Data: ${response.body}")
        }
        is NetworkResponse.Error -> {
            // An error occurred, display the error message
            println("Error: ${response.code} - ${response.message}")
        }
        is NetworkResponse.Loading -> {
            // The request is still in progress, display a loading indicator
            println("Loading...")
        }
    }
}

```

**Explanation:**

* The code defines a sealed class called `NetworkResponse` to represent the different types of responses that can be received from a network request. It includes three subclasses: `Success`, `Error`, and `Loading`.
* The `fetchApiData()` function is a suspend function that simulates a network request by suspending the coroutine for a short time and then randomly generating a response.
* The `generateRandomData()` function is a helper function that generates random data of type `T`.
* The `scope` variable is a coroutine scope that is used to launch a new coroutine.
* The `launch` function is used to launch a new coroutine that calls the `fetchApiData()` function and handles the result.

This code demonstrates the use of sealed classes, suspend functions, coroutines, and random data generation in Kotlin.