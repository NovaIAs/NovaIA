```go
// This is a simple program that implements a basic web server using the Gorilla Mux router.
// It serves static files from the "public" directory and provides a REST API for managing
// a list of tasks.

package main

import (
	"encoding/json"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/gorilla/mux"
)

// Task represents a single task in the to-do list.
type Task struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	Completed bool   `json:"completed"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// Tasks is a slice of tasks that represents the to-do list.
var tasks = []Task{
	{ID: 1, Name: "Buy groceries", Completed: false, CreatedAt: time.Now(), UpdatedAt: time.Now()},
	{ID: 2, Name: "Clean the house", Completed: false, CreatedAt: time.Now(), UpdatedAt: time.Now()},
	{ID: 3, Name: "Go for a run", Completed: true, CreatedAt: time.Now(), UpdatedAt: time.Now()},
}

// main function starts the web server and handles the HTTP requests.
func main() {
	// Create a new router.
	r := mux.NewRouter()

	// Serve static files from the "public" directory.
	r.PathPrefix("/public/").Handler(http.StripPrefix("/public/", http.FileServer(http.Dir("public"))))

	// Define the REST API routes.
	api := r.PathPrefix("/api/v1").Subrouter()
	api.HandleFunc("/tasks", getTasks).Methods(http.MethodGet)
	api.HandleFunc("/tasks", createTask).Methods(http.MethodPost)
	api.HandleFunc("/tasks/{id}", getTask).Methods(http.MethodGet)
	api.HandleFunc("/tasks/{id}", updateTask).Methods(http.MethodPut)
	api.HandleFunc("/tasks/{id}", deleteTask).Methods(http.MethodDelete)

	// Start the web server on port 8080.
	log.Fatal(http.ListenAndServe(":8080", r))
}

// getTasks function returns a list of all the tasks in the to-do list.
func getTasks(w http.ResponseWriter, r *http.Request) {
	// Set the CORS headers.
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Return the list of tasks as JSON.
	json.NewEncoder(w).Encode(tasks)
}

// createTask function creates a new task and adds it to the to-do list.
func createTask(w http.ResponseWriter, r *http.Request) {
	// Set the CORS headers.
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Decode the task from the request body.
	var task Task
	if err := json.NewDecoder(r.Body).Decode(&task); err != nil {
		http.Error(w, "Invalid task", http.StatusBadRequest)
		return
	}

	// Generate a new ID for the task.
	task.ID = len(tasks) + 1

	// Set the task's creation and update timestamps.
	task.CreatedAt = time.Now()
	task.UpdatedAt = time.Now()

	// Add the task to the to-do list.
	tasks = append(tasks, task)

	// Return the created task as JSON.
	json.NewEncoder(w).Encode(task)
}

// getTask function returns a single task by its ID.
func getTask(w http.ResponseWriter, r *http.Request) {
	// Set the CORS headers.
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Get the task ID from the request parameters.
	id, err := strconv.Atoi(mux.Vars(r)["id"])
	if err != nil {
		http.Error(w, "Invalid task ID", http.StatusBadRequest)
		return
	}

	// Find the task by its ID.
	var task Task
	found := false
	for _, t := range tasks {
		if t.ID == id {
			task = t
			found = true
			break
		}
	}

	// If the task was not found, return an error.
	if !found {
		http.Error(w, "Task not found", http.StatusNotFound)
		return
	}

	// Return the task as JSON.
	json.NewEncoder(w).Encode(task)
}

// updateTask function updates a single task by its ID.
func updateTask(w http.ResponseWriter, r *http.Request) {
	// Set the CORS headers.
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Get the task ID from the request parameters.
	id, err := strconv.Atoi(mux.Vars(r)["id"])
	if err != nil {
		http.Error(w, "Invalid task ID", http.StatusBadRequest)
		return
	}

	// Find the task by its ID.
	var task Task
	found := false
	for i, t := range tasks {
		if t.ID == id {
			task = t
			found = true
			break
		}
	}

	// If the task was not found, return an error.
	if !found {
		http.Error(w, "Task not found", http.StatusNotFound)
		return