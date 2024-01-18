```go
// Import necessary libraries
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

// Define a struct to represent a lock
type Lock struct {
	sync.RWMutex
	expiry time.Time
}

// Create a map to store the locks
var lockMap = make(map[string]*Lock)

// Our main function
func main() {
	// Start a web server on port 8080
	r := mux.NewRouter()
	r.HandleFunc("/lock", lockHandler)
	r.HandleFunc("/unlock", unlockHandler)
	r.HandleFunc("/check", checkHandler)

	srv := &http.Server{
		Handler: r,
		Addr:    ":8080",
	}

	// Start the server in a goroutine
	go func() {
		if err := srv.ListenAndServe(); err != nil {
			log.Fatal(err)
		}
	}()

	// Keep the main goroutine alive
	select {}
}

// Handle requests to acquire a lock
func lockHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	var body map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Get the lock name
	lockName, ok := body["lock_name"]
	if !ok {
		http.Error(w, "Missing lock_name field", http.StatusBadRequest)
		return
	}

	// Create a new lock if it doesn't exist
	lock := getLock(lockName.(string))

	// Acquire the write lock
	lock.Lock()
	defer lock.Unlock()

	// Set the expiry time
	lock.expiry = time.Now().Add(time.Minute)

	// Send a success response
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Lock acquired"))
}

// Handle requests to release a lock
func unlockHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	var body map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Get the lock name
	lockName, ok := body["lock_name"]
	if !ok {
		http.Error(w, "Missing lock_name field", http.StatusBadRequest)
		return
	}

	// Get the lock
	lock := getLock(lockName.(string))

	// Acquire the write lock
	lock.Lock()
	defer lock.Unlock()

	// Reset the expiry time
	lock.expiry = time.Time{}

	// Send a success response
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Lock released"))
}

// Handle requests to check if a lock is held
func checkHandler(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	var body map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Get the lock name
	lockName, ok := body["lock_name"]
	if !ok {
		http.Error(w, "Missing lock_name field", http.StatusBadRequest)
		return
	}

	// Get the lock
	lock := getLock(lockName.(string))

	// Acquire the read lock
	lock.RLock()
	defer lock.RUnlock()

	// Check if the lock is held
	if lock.expiry.After(time.Now()) {
		// Lock is held
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("Lock held"))
	} else {
		// Lock is not held
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("Lock not held"))
	}
}

// Helper function to get a lock
func getLock(name string) *Lock {
	// Check if the lock exists
	lock, ok := lockMap[name]
	if !ok {
		// Create a new lock
		lock = &Lock{
			expiry: time.Time{},
		}

		// Add the lock to the map
		lockMap[name] = lock
	}

	return lock
}
```

**Explanation:**

- The code defines a `Lock` struct representing a lock, `lockMap` to store locks, and a lock acquisition timeout of 1 minute.


- The code creates an HTTP server listening on port 8080 to handle lock acquisition, release, and checking requests.


- The `lockHandler` function acquires a write lock, sets an expiry time, and returns a success response.


- The `unlockHandler` function releases a lock and resets the expiry time.


- The `checkHandler` function checks if a lock is held and returns the result.


- The `getLock` function retrieves or creates a lock with the given name.