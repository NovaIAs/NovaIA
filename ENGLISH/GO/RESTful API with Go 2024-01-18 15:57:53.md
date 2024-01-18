```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/joho/godotenv"
)

// Define the port for the HTTP server.
const port = ":8080"

// Define the structure for the data model.
type Data struct {
	ID        int       `json:"id"`
	Name      string    `json:"name"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// Define the slice of data for the API.
var data []Data

// Define the handler for the GET /data route.
func getDataHandler(w http.ResponseWriter, r *http.Request) {
	// Write the data to the response.
	if err := RespondWithJSON(w, http.StatusOK, data); err != nil {
		log.Println(err)
		http.Error(w, http.StatusInternalServerError.String(), http.StatusInternalServerError)
		return
	}
}

// Define the handler for the POST /data route.
func postDataHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body.
	var newData Data
	if err := ParseJSON(r.Body, &newData); err != nil {
		log.Println(err)
		http.Error(w, http.StatusBadRequest.String(), http.StatusBadRequest)
		return
	}

	// Validate the data.
	if err := newData.Validate(); err != nil {
		log.Println(err)
		http.Error(w, http.StatusBadRequest.String(), http.StatusBadRequest)
		return
	}

	// Add the new data to the slice.
	data = append(data, newData)

	// Write the new data to the response.
	if err := RespondWithJSON(w, http.StatusCreated, newData); err != nil {
		log.Println(err)
		http.Error(w, http.StatusInternalServerError.String(), http.StatusInternalServerError)
		return
	}
}

// Define the handler for the PUT /data/:id route.
func putDataHandler(w http.ResponseWriter, r *http.Request) {
	// Get the id from the request.
	id := mux.Vars(r)["id"]

	// Find the data by id.
	var foundData Data
	found := false
	for _, d := range data {
		if d.ID == id {
			foundData = d
			found = true
			break
		}
	}

	// If the data was not found, return an error.
	if !found {
		http.Error(w, http.StatusNotFound.String(), http.StatusNotFound)
		return
	}

	// Parse the request body.
	var updatedData Data
	if err := ParseJSON(r.Body, &updatedData); err != nil {
		log.Println(err)
		http.Error(w, http.StatusBadRequest.String(), http.StatusBadRequest)
		return
	}

	// Validate the data.
	if err := updatedData.Validate(); err != nil {
		log.Println(err)
		http.Error(w, http.StatusBadRequest.String(), http.StatusBadRequest)
		return
	}

	// Update the data.
	foundData.Name = updatedData.Name
	foundData.UpdatedAt = time.Now()

	// Write the updated data to the response.
	if err := RespondWithJSON(w, http.StatusOK, foundData); err != nil {
		log.Println(err)
		http.Error(w, http.StatusInternalServerError.String(), http.StatusInternalServerError)
		return
	}
}

// Define the handler for the DELETE /data/:id route.
func deleteDataHandler(w http.ResponseWriter, r *http.Request) {
	// Get the id from the request.
	id := mux.Vars(r)["id"]

	// Find the data by id.
	var foundData Data
	found := false
	for i, d := range data {
		if d.ID == id {
			foundData = d
			found = true
			data = append(data[:i], data[i+1:]...)
			break
		}
	}

	// If the data was not found, return an error.
	if !found {
		http.Error(w, http.StatusNotFound.String(), http.StatusNotFound)
		return
	}

	// Write the deleted data to the response.
	if err := RespondWithJSON(w, http.StatusOK, foundData); err != nil {
		log.Println(err)
		http.Error(w, http.StatusInternalServerError.String(), http.StatusInternalServerError)
		return
	}
}

// Define the main function.
func main() {
	// Load the environment variables.
	if err := godotenv.Load(); err != nil {
		log.Println(err)
		return
	}

	// Initialize the data.
	data = []Data{
		{ID: 1, Name: "John Doe", CreatedAt: time.Now(), UpdatedAt: time.Now()},
		{ID: 2, Name: "Jane Doe", CreatedAt: time.Now(), UpdatedAt: time.Now()},
	}

	// Create a new router.
	router := mux.NewRouter()

	// Define the routes.
	router.HandleFunc("/data", getDataHandler).Methods(http.MethodGet)
	router.HandleFunc("/data", postDataHandler).Methods(http.MethodPost)
	router.HandleFunc("/data/{id}", putDataHandler).Methods(http.MethodPut)
	router.HandleFunc("/data/{id}", deleteDataHandler).Methods(http.MethodDelete)

	// Start the HTTP server.
	log.Println(fmt.Sprintf("Server listening on port %s", port))
	if err := http.ListenAndServe(port, router); err != nil {
		log.Println(err)
		os.Exit(1)
	}
}

// Define the helper functions.

// RespondWithJSON writes the given data to the response in JSON format.
func RespondWithJSON(w http.ResponseWriter, code int, data interface{}) error {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	return json.NewEncoder(w).Encode(data)
}

// ParseJSON parses the request body into the given interface.
func ParseJSON(r io.Reader, v interface{}) error {
	dec := json.NewDecoder(r)