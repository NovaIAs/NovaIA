```go
package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
)

type Message struct {
	ID      string    `json:"id"`
	Content string    `json:"content"`
	Author  string    `json:"author"`
	Created time.Time `json:"created"`
}

var messages []Message

func main() {
	// Initialize the messages array with some sample data
	messages = []Message{
		{ID: "1", Content: "Hello, world!", Author: "John Doe", Created: time.Now()},
		{ID: "2", Content: "This is a test message", Author: "Jane Smith", Created: time.Now()},
	}

	// Create a new Gorilla Mux router
	router := mux.NewRouter()

	// Define the routes for the API
	router.HandleFunc("/messages", GetMessages).Methods("GET")
	router.HandleFunc("/messages/{id}", GetMessage).Methods("GET")
	router.HandleFunc("/messages", CreateMessage).Methods("POST")
	router.HandleFunc("/messages/{id}", UpdateMessage).Methods("PUT")
	router.HandleFunc("/messages/{id}", DeleteMessage).Methods("DELETE")

	// Start the HTTP server
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}

	log.Printf("Starting server on port %s", port)
	log.Fatal(http.ListenAndServe(":"+port, router))
}

// GetMessages returns all messages
func GetMessages(w http.ResponseWriter, r *http.Request) {
	// Set the content type to JSON
	w.Header().Set("Content-Type", "application/json")

	// Marshal the messages array into JSON
	json, err := json.Marshal(messages)
	if err != nil {
		log.Printf("Error marshalling messages: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	// Write the JSON response to the client
	w.Write(json)
}

// GetMessage returns a single message
func GetMessage(w http.ResponseWriter, r *http.Request) {
	// Set the content type to JSON
	w.Header().Set("Content-Type", "application/json")

	// Get the message ID from the request parameters
	id := mux.Vars(r)["id"]

	// Find the message with the given ID
	var message Message
	found := false
	for _, m := range messages {
		if m.ID == id {
			message = m
			found = true
			break
		}
	}

	// If the message was not found, return a 404 Not Found error
	if !found {
		http.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	// Marshal the message into JSON
	json, err := json.Marshal(message)
	if err != nil {
		log.Printf("Error marshalling message: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	// Write the JSON response to the client
	w.Write(json)
}

// CreateMessage creates a new message
func CreateMessage(w http.ResponseWriter, r *http.Request) {
	// Set the content type to JSON
	w.Header().Set("Content-Type", "application/json")

	// Decode the request body into a Message struct
	var message Message
	err := json.NewDecoder(r.Body).Decode(&message)
	if err != nil {
		log.Printf("Error decoding request body: %v", err)
		http.Error(w, "Bad Request", http.StatusBadRequest)
		return
	}

	// Generate a random ID for the message
	message.ID = fmt.Sprintf("%d", rand.Int63())

	// Add the message to the messages array
	messages = append(messages, message)

	// Marshal the message into JSON
	json, err := json.Marshal(message)
	if err != nil {
		log.Printf("Error marshalling message: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	// Write the JSON response to the client
	w.Write(json)
}

// UpdateMessage updates an existing message
func UpdateMessage(w http.ResponseWriter, r *http.Request) {
	// Set the content type to JSON
	w.Header().Set("Content-Type", "application/json")

	// Get the message ID from the request parameters
	id := mux.Vars(r)["id"]

	// Find the message with the given ID
	var message Message
	found := false
	for i, m := range messages {
		if m.ID == id {
			message = m
			found = true
			break
		}
	}

	// If the message was not found, return a 404 Not Found error
	if !found {
		http.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	// Decode the request body into a Message struct
	err := json.NewDecoder(r.Body).Decode(&message)
	if err != nil {
		log.Printf("Error decoding request body: %v", err)
		http.Error(w, "Bad Request", http.StatusBadRequest)
		return
	}

	// Update the message in the messages array
	messages[i] = message

	// Marshal the message into JSON
	json, err := json.Marshal(message)
	if err != nil {
		log.Printf("Error marshalling message: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	// Write the JSON response to the client
	w.Write(json)
}

// DeleteMessage deletes an existing message
func DeleteMessage(w http.ResponseWriter, r *http.Request) {
	// Set the content type to JSON
	w.Header().Set("Content-Type", "application/json")

	// Get the message ID from the request parameters
	id := mux.Vars(r)["id"]

	// Find the message with the given ID
	var message Message
	found := false
	for i, m := range messages {
		if m.ID == id {
			message = m
			found = true
			break