```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
)

// Define a new type called "Task" with two fields: "ID" and "Description".
type Task struct {
	ID          string    `json:"id"`
	Description string    `json:"description"`
	CreatedAt   time.Time `json:"created_at"`
}

// Create a global variable to store a list of tasks.
var tasks []Task

// Function to create a new task and add it to the list of tasks.
func CreateTask(w http.ResponseWriter, r *http.Request) {
	// Decode the JSON request body into a Task object.
	var task Task
	err := json.NewDecoder(r.Body).Decode(&task)
	if err != nil {
		log.Printf("Error decoding JSON request body: %v", err)
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Generate a unique ID for the task.
	task.ID = uuid.NewV4().String()

	// Set the creation timestamp.
	task.CreatedAt = time.Now()

	// Add the task to the list of tasks.
	tasks = append(tasks, task)

	// Encode the task object as JSON and write it to the response body.
	json.NewEncoder(w).Encode(task)
}

// Function to get all tasks from the list of tasks.
func GetTasks(w http.ResponseWriter, r *http.Request) {
	// Encode the list of tasks as JSON and write it to the response body.
	json.NewEncoder(w).Encode(tasks)
}

// Function to get a single task from the list of tasks.
func GetTask(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters.
	taskID := mux.Vars(r)["id"]

	// Search for the task in the list of tasks.
	var task Task
	found := false
	for _, t := range tasks {
		if t.ID == taskID {
			task = t
			found = true
			break
		}
	}

	// If the task was not found, return a 404 error.
	if !found {
		http.Error(w, "Task not found", http.StatusNotFound)
		return
	}

	// Encode the task object as JSON and write it to the response body.
	json.NewEncoder(w).Encode(task)
}

// Function to update a task in the list of tasks.
func UpdateTask(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters.
	taskID := mux.Vars(r)["id"]

	// Decode the JSON request body into a Task object.
	var task Task
	err := json.NewDecoder(r.Body).Decode(&task)
	if err != nil {
		log.Printf("Error decoding JSON request body: %v", err)
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	// Search for the task in the list of tasks.
	var updatedTask Task
	found := false
	for i, t := range tasks {
		if t.ID == taskID {
			updatedTask = t
			found = true
			tasks[i] = task
			break
		}
	}

	// If the task was not found, return a 404 error.
	if !found {
		http.Error(w, "Task not found", http.StatusNotFound)
		return
	}

	// Encode the updated task object as JSON and write it to the response body.
	json.NewEncoder(w).Encode(updatedTask)
}

// Function to delete a task from the list of tasks.
func DeleteTask(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters.
	taskID := mux.Vars(r)["id"]

	// Search for the task in the list of tasks.
	found := false
	for i, t := range tasks {
		if t.ID == taskID {
			found = true
			tasks = append(tasks[:i], tasks[i+1:]...)
			break
		}
	}

	// If the task was not found, return a 404 error.
	if !found {
		http.Error(w, "Task not found", http.StatusNotFound)
		return
	}

	// Write a success message to the response body.
	fmt.Fprint(w, "Task deleted successfully")
}

// Main function to start the HTTP server.
func main() {
	// Create a new router using the Gorilla Mux package.
	router := mux.NewRouter()

	// Define the routes for the API.
	router.HandleFunc("/tasks", CreateTask).Methods("POST")
	router.HandleFunc("/tasks", GetTasks).Methods("GET")
	router.HandleFunc("/tasks/{id}", GetTask).Methods("GET")
	router.HandleFunc("/tasks/{id}", UpdateTask).Methods("PUT")
	router.HandleFunc("/tasks/{id}", DeleteTask).Methods("DELETE")

	// Start the HTTP server on port 8080.
	log.Fatal(http.ListenAndServe(":8080", router))
}
```

Explanation:

This Go code creates a simple REST API for managing a list of tasks using the Gorilla Mux library.

* We define a Task struct with fields "ID", "Description", and "CreatedAt".
* We create a global variable "tasks" to store a list of tasks.
* The CreateTask function creates a new task, generates a unique ID, sets the creation timestamp, and adds it to the list of tasks.
* The GetTasks function returns all tasks in the list.
* The GetTask function gets a single task from the list by its ID.
* The UpdateTask function updates an existing task in the list.
* The DeleteTask function deletes a task from the list by its ID.
* The main function creates a router using Gorilla Mux, defines the API routes, and starts the HTTP server on port 8080.

This code demonstrates how to work with HTTP requests, JSON data, and a slice of structs to build a simple API. It also shows how to use UUIDs for generating unique IDs and how to handle different HTTP methods (POST, GET, PUT, and DELETE) to perform different operations on the data.