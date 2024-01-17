```go
package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gorilla/mux"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

const (
	dbName   = "mydb"
	user     = "myuser"
	password = "mypassword"
)

var dbPool *pgxpool.Pool

func main() {
	// Connect to the PostgreSQL database.
	dbURI := fmt.Sprintf("host=localhost user=%s password=%s database=%s", user, password, dbName)
	var err error
	dbPool, err = pgxpool.Connect(context.Background(), dbURI)
	if err != nil {
		log.Fatal(err)
	}

	// Create a new HTTP router.
	router := mux.NewRouter()

	// Define HTTP handlers.
	router.HandleFunc("/", indexHandler).Methods(http.MethodGet)
	router.HandleFunc("/users", getAllUsersHandler).Methods(http.MethodGet)
	router.HandleFunc("/users/{id}", getUserByIDHandler).Methods(http.MethodGet)
	router.HandleFunc("/users", createUserHandler).Methods(http.MethodPost)
	router.HandleFunc("/users/{id}", updateUserHandler).Methods(http.MethodPut)
	router.HandleFunc("/users/{id}", deleteUserHandler).Methods(http.MethodDelete)

	// Start the HTTP server.
	srv := &http.Server{
		Addr:    ":8080",
		Handler: router,
	}

	// Gracefully shutdown the HTTP server when Ctrl+C is pressed.
	done := make(chan os.Signal, 1)
	signal.Notify(done, os.Interrupt, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-done
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			log.Fatal(err)
		}
	}()

	log.Println("Server is listening on port 8080")
	if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatal(err)
	}
}

// indexHandler responds to requests on the root URL.
func indexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, world!")
}

// getAllUsersHandler returns all users in the database.
func getAllUsersHandler(w http.ResponseWriter, r *http.Request) {
	rows, err := dbPool.Query(context.Background(), "SELECT * FROM users")
	if err != nil {
		http.Error(w, "Error getting all users", http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var users []User
	for rows.Next() {
		var user User
		if err := rows.Scan(&user.ID, &user.Name, &user.Email); err != nil {
			http.Error(w, "Error scanning user", http.StatusInternalServerError)
			return
		}
		users = append(users, user)
	}

	if err := rows.Err(); err != nil {
		http.Error(w, "Error getting all users", http.StatusInternalServerError)
		return
	}

	jsonResponse, err := json.Marshal(users)
	if err != nil {
		http.Error(w, "Error marshalling users to JSON", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

// getUserByIDHandler returns a user by their ID.
func getUserByIDHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	row := dbPool.QueryRow(context.Background(), "SELECT * FROM users WHERE id=$1", id)
	var user User
	if err := row.Scan(&user.ID, &user.Name, &user.Email); err != nil {
		if err == pgx.ErrNoRows {
			http.Error(w, "User not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Error getting user by ID", http.StatusInternalServerError)
		return
	}

	jsonResponse, err := json.Marshal(user)
	if err != nil {
		http.Error(w, "Error marshalling user to JSON", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

// createUserHandler creates a new user.
func createUserHandler(w http.ResponseWriter, r *http.Request) {
	var user User
	if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
		http.Error(w, "Error decoding JSON request body", http.StatusBadRequest)
		return
	}

	if err := dbPool.QueryRow(context.Background(), "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", user.Name, user.Email).Scan(&user.ID); err != nil {
		http.Error(w, "Error creating user", http.StatusInternalServerError)
		return
	}

	jsonResponse, err := json.Marshal(user)
	if err != nil {
		http.Error(w, "Error marshalling user to JSON", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

// updateUserHandler updates a user by their ID.
func updateUserHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var user User
	if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
		http.Error(w, "Error decoding JSON request body", http.StatusBadRequest)
		return
	}

	if err := dbPool.QueryRow(context.Background(), "UPDATE users SET name=$1, email=$2 WHERE id=$3 RETURNING id", user.Name, user.Email, id).Scan(&user.ID); err != nil {
		if err == pgx.ErrNoRows {
			http.Error(w, "User not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Error updating user", http.StatusInternalServerError)
		return
	}

	jsonResponse, err := json.Marshal(user)
	if err != nil {