```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

// MyStruct is a custom struct
type MyStruct struct {
	Name string
	Age  int
}

// Global variables
var (
	port = os.Getenv("PORT") // Get the port from the environment variable
	data = make(map[string]MyStruct) // A map to store data
	mu   sync.Mutex                     // A mutex to protect the data map
)

// HandlerFunc is a type alias for a function that handles HTTP requests
type HandlerFunc func(http.ResponseWriter, *http.Request)

// IndexHandler handles the root URL
func IndexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, world!")
}

// DataHandler handles requests to the /data URL
func DataHandler(w http.ResponseWriter, r *http.Request) {
	// Get the data from the map
	mu.Lock()
	defer mu.Unlock()
	data := data[r.URL.Path]

	// Write the data to the response
	fmt.Fprintf(w, "%s is %d years old", data.Name, data.Age)
}

// AddDataHandler handles requests to the /data/add URL
func AddDataHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	if err := r.ParseForm(); err != nil {
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Get the data from the request
	name := r.FormValue("name")
	age, err := strconv.Atoi(r.FormValue("age"))
	if err != nil {
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Add the data to the map
	mu.Lock()
	defer mu.Unlock()
	data[r.URL.Path] = MyStruct{Name: name, Age: age}

	// Write a success message to the response
	fmt.Fprintf(w, "Data added successfully")
}

// UpdateDataHandler handles requests to the /data/update URL
func UpdateDataHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	if err := r.ParseForm(); err != nil {
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Get the data from the request
	name := r.FormValue("name")
	age, err := strconv.Atoi(r.FormValue("age"))
	if err != nil {
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Update the data in the map
	mu.Lock()
	defer mu.Unlock()
	data[r.URL.Path] = MyStruct{Name: name, Age: age}

	// Write a success message to the response
	fmt.Fprintf(w, "Data updated successfully")
}

// DeleteDataHandler handles requests to the /data/delete URL
func DeleteDataHandler(w http.ResponseWriter, r *http.Request) {
	// Delete the data from the map
	mu.Lock()
	defer mu.Unlock()
	delete(data, r.URL.Path)

	// Write a success message to the response
	fmt.Fprintf(w, "Data deleted successfully")
}

// HealthCheckHandler handles requests to the /healthcheck URL
func HealthCheckHandler(w http.ResponseWriter, r *http.Request) {
	// Check if the data map is empty
	mu.Lock()
	defer mu.Unlock()
	isEmpty := len(data) == 0

	// Write the health check status to the response
	if isEmpty {
		fmt.Fprintf(w, "Service is healthy")
	} else {
		fmt.Fprintf(w, "Service is unhealthy")
	}
}

func main() {
	// Create a new router
	router := mux.NewRouter()

	// Define the routes
	router.HandleFunc("/", IndexHandler)
	router.HandleFunc("/data", DataHandler)
	router.HandleFunc("/data/add", AddDataHandler)
	router.HandleFunc("/data/update", UpdateDataHandler)
	router.HandleFunc("/data/delete", DeleteDataHandler)
	router.HandleFunc("/healthcheck", HealthCheckHandler)

	// Start the server
	srv := &http.Server{
		Handler:      router,
		Addr:         ":" + port,
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 30 * time.Second,
	}

	// Log a message when the server starts
	log.Printf("Server listening on port %s", port)

	// Start the server in a goroutine
	go func() {
		if err := srv.ListenAndServe(); err != nil {
			log.Fatal(err)
		}
	}()

	// Wait for the server to start
	time.Sleep(1 * time.Second)

	// Add some data to the map
	mu.Lock()
	data["/john"] = MyStruct{Name: "John Doe", Age: 30}
	data["/jane"] = MyStruct{Name: "Jane Doe", Age: 25}
	mu.Unlock()

	// Keep the server running until the user presses Ctrl+C
	select {}
}
```

This code sets up a simple HTTP server using the Gorilla Mux router. It defines five routes:

1. `/`: IndexHandler - Handles the root URL and returns "Hello, world!".
2. `/data`: DataHandler - Handles requests to get data from a map.
3. `/data/add`: AddDataHandler - Handles requests to add data to the map.
4. `/data/update`: UpdateDataHandler - Handles requests to update data in the map.
5. `/data/delete`: DeleteDataHandler - Handles requests to delete data from the map.
6. `/healthcheck`: HealthCheckHandler - Handles requests to check the health of the service.

The server uses a mutex to protect the data map from concurrent access.

Here's a breakdown of some of the key parts of the code:

* `data` is a global map that stores the data in the form of `MyStruct`, which has two fields: `Name` and `Age`.
* `mu` is a global mutex that protects the `data` map from concurrent access.
* The `main` function is the entry point of the program. It sets up the router and starts the HTTP server.
* The `IndexHandler` function handles the root URL and returns "Hello, world!".
* The `DataHandler` function handles requests to get data from the `data` map.
* The `AddDataHandler` function handles requests to add data to the `data` map.
* The `UpdateDataHandler` function handles requests to update data in the `data` map.
* The `DeleteDataHandler` function handles requests to delete data from the `data` map.
* The `HealthCheckHandler` function handles requests to check the health of the service. It checks if the `data` map is empty and returns a message accordingly.

This code demonstrates how to use the Gorilla Mux router to create an HTTP server with multiple routes and how to use a mutex to protect shared data from concurrent access.