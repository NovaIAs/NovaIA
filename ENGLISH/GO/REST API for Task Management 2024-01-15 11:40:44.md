```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/gorilla/mux"
)

// Constants for HTTP status codes
const (
	StatusOK           = 200
	StatusCreated       = 201
	StatusBadRequest    = 400
	StatusUnauthorized  = 401
	StatusForbidden     = 403
	StatusNotFound      = 404
	StatusInternalError = 500
)

// Struct to represent a task
type Task struct {
	ID        int    `json:"id"`
	Title     string `json:"title"`
	Completed bool   `json:"completed"`
}

// In-memory storage for tasks
var tasks []Task

// CreateTaskHandler handles the creation of a new task
func CreateTaskHandler(w http.ResponseWriter, r *http.Request) {
	// Decode the JSON payload into a Task struct
	var task Task
	if err := json.NewDecoder(r.Body).Decode(&task); err != nil {
		http.Error(w, "Invalid JSON payload", StatusBadRequest)
		return
	}

	// Generate a new ID for the task
	task.ID = len(tasks) + 1

	// Add the task to the in-memory storage
	tasks = append(tasks, task)

	// Respond with the newly created task
	w.WriteHeader(StatusCreated)
	if err := json.NewEncoder(w).Encode(task); err != nil {
		http.Error(w, "Error encoding task", StatusInternalError)
		return
	}
}

// GetTasksHandler handles the retrieval of all tasks
func GetTasksHandler(w http.ResponseWriter, r *http.Request) {
	// Respond with the list of tasks
	w.WriteHeader(StatusOK)
	if err := json.NewEncoder(w).Encode(tasks); err != nil {
		http.Error(w, "Error encoding tasks", StatusInternalError)
		return
	}
}

// GetTaskHandler handles the retrieval of a single task
func GetTaskHandler(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid task ID", StatusBadRequest)
		return
	}

	// Find the task by ID
	task, err := findTaskByID(id)
	if err != nil {
		http.Error(w, "Task not found", StatusNotFound)
		return
	}

	// Respond with the task
	w.WriteHeader(StatusOK)
	if err := json.NewEncoder(w).Encode(task); err != nil {
		http.Error(w, "Error encoding task", StatusInternalError)
		return
	}
}

// UpdateTaskHandler handles the updating of a task
func UpdateTaskHandler(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid task ID", StatusBadRequest)
		return
	}

	// Decode the JSON payload into a Task struct
	var task Task
	if err := json.NewDecoder(r.Body).Decode(&task); err != nil {
		http.Error(w, "Invalid JSON payload", StatusBadRequest)
		return
	}

	// Find the task by ID
	task, err = findTaskByID(id)
	if err != nil {
		http.Error(w, "Task not found", StatusNotFound)
		return
	}

	// Update the task
	task.Title = task.Title
	task.Completed = task.Completed

	// Respond with the updated task
	w.WriteHeader(StatusOK)
	if err := json.NewEncoder(w).Encode(task); err != nil {
		http.Error(w, "Error encoding task", StatusInternalError)
		return
	}
}

// DeleteTaskHandler handles the deletion of a task
func DeleteTaskHandler(w http.ResponseWriter, r *http.Request) {
	// Get the task ID from the request parameters
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid task ID", StatusBadRequest)
		return
	}

	// Find the task by ID
	task, err := findTaskByID(id)
	if err != nil {
		http.Error(w, "Task not found", StatusNotFound)
		return
	}

	// Delete the task from the in-memory storage
	tasks = deleteTaskByID(id)

	// Respond with the deleted task
	w.WriteHeader(StatusOK)
	if err := json.NewEncoder(w).Encode(task); err != nil {
		http.Error(w, "Error encoding task", StatusInternalError)
		return
	}
}

// findTaskByID finds a task by its ID
func findTaskByID(id int) (Task, error) {
	for _, task := range tasks {
		if task.ID == id {
			return task, nil
		}
	}

	return Task{}, fmt.Errorf("Task not found")
}

// deleteTaskByID deletes a task by its ID
func deleteTaskByID(id int) []Task {
	var newTasks []Task

	for _, task := range tasks {
		if task.ID != id {
			newTasks = append(newTasks, task)
		}
	}

	return newTasks
}

func main() {
	// Create a new router
	router := mux.NewRouter()

	// Define the API routes
	router.HandleFunc("/tasks", CreateTaskHandler).Methods("POST")
	router.HandleFunc("/tasks", GetTasksHandler).Methods("GET")
	router.HandleFunc("/tasks/{id}", GetTaskHandler).Methods("GET")
	router.HandleFunc("/tasks/{id}", UpdateTaskHandler).Methods("PUT")
	router.HandleFunc("/tasks/{id}", DeleteTaskHandler).Methods("DELETE")

	// Start the HTTP server
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}
	log.Printf("Listening on port %s", port)
	if err := http.ListenAndServe(":"+port, router); err != nil {
		log.Fatal(err)
	}
}
```

This code is a REST API written in Go that allows you to create, retrieve, update, and delete tasks. It uses the Gorilla Mux library for routing and JSON encoding/decoding. The tasks are stored in an in-memory slice, which is not a scalable solution for a real-world application, but it's enough for this example.

The main function creates a new router, defines the API routes, and starts the HTTP server. The API routes are defined using the `HandleFunc` function, which takes a route pattern and a handler function. The handler function is a function that handles the request and writes the response.

The handler functions for each route are defined in the `CreateTaskHandler`, `GetTasksHandler`, `GetTaskHandler`, `UpdateTaskHandler`, and `DeleteTaskHandler` functions. These functions use the `json` package to encode and decode the JSON payload of the request and response. They also use the `findTaskByID` and `deleteTaskByID` functions to find and delete tasks by their ID.

The `findTaskByID` function takes an ID as an argument and returns the task with that ID, or an error if the task is not found. The `deleteTaskByID` function takes an ID as an argument and returns a new slice of tasks with the task with that ID removed.

The `main` function also starts the HTTP server on the port specified by the `PORT` environment variable, or on port 8080 if the environment variable is not set.