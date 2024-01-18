```go

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"time"

	"cloud.google.com/go/bigquery"
	"cloud.google.com/go/pubsub"
	"golang.org/x/crypto/bcrypt"
)

// User represents a user in the database.
type User struct {
	Email    string `bigquery:"email"`
	Name     string `bigquery:"name"`
	Password string `bigquery:"password"`
}

// Message represents a message sent to a Pub/Sub topic.
type Message struct {
	Data []byte `json:"data"`
}

// PageData represents the data to be passed to the HTML template.
type PageData struct {
	Users []User
}

const (
	bigQueryDatasetID = "users"
	bigQueryTableID   = "user_data"

	pubSubTopicID = "user_data"
)

var (
	bigqueryClient *bigquery.Client
	pubSubClient   *pubsub.Client
	htmlTemplate  *template.Template
)

func main() {
	var err error

	// Initialize the BigQuery client.
	bigqueryClient, err = bigquery.NewClient(context.Background(), "")
	if err != nil {
		log.Fatalf("Failed to create BigQuery client: %v", err)
	}

	// Initialize the Pub/Sub client.
	pubSubClient, err = pubsub.NewClient(context.Background(), "")
	if err != nil {
		log.Fatalf("Failed to create Pub/Sub client: %v", err)
	}

	// Initialize the HTML template.
	htmlTemplate = template.Must(template.ParseFiles("index.html"))

	// Start the HTTP server.
	http.HandleFunc("/", indexHandler)
	http.HandleFunc("/users", usersHandler)
	http.HandleFunc("/create_user", createUserHandler)
	http.HandleFunc("/delete_user", deleteUserHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}

// indexHandler displays the homepage.
func indexHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	htmlTemplate.Execute(w, nil)
}

// usersHandler returns a list of all users in the database.
func usersHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/users" {
		http.NotFound(w, r)
		return
	}

	// Get all users from BigQuery.
	query := bigqueryClient.Query("SELECT email, name FROM `" + bigQueryDatasetID + "." + bigQueryTableID + "`")
	it, err := query.Read(context.Background())
	if err != nil {
		http.Error(w, "Failed to query BigQuery.", http.StatusInternalServerError)
		log.Printf("Failed to query BigQuery: %v", err)
		return
	}

	// Parse the BigQuery results.
	var users []User
	for {
		var user User
		err := it.Next(&user)
		if err == iterator.Done {
			break
		}
		if err != nil {
			http.Error(w, "Failed to parse BigQuery results.", http.StatusInternalServerError)
			log.Printf("Failed to parse BigQuery results: %v", err)
			return
		}
		users = append(users, user)
	}

	// Render the HTML template.
	data := PageData{Users: users}
	if err := htmlTemplate.Execute(w, data); err != nil {
		http.Error(w, "Failed to render HTML template.", http.StatusInternalServerError)
		log.Printf("Failed to render HTML template: %v", err)
	}
}

// createUserHandler creates a new user in the database.
func createUserHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Invalid request method.", http.StatusMethodNotAllowed)
		return
	}

	// Parse the form data.
	if err := r.ParseForm(); err != nil {
		http.Error(w, "Failed to parse form data.", http.StatusBadRequest)
		log.Printf("Failed to parse form data: %v", err)
		return
	}
	email := r.FormValue("email")
	name := r.FormValue("name")
	password := r.FormValue("password")

	// Validate the form data.
	if email == "" || name == "" || password == "" {
		http.Error(w, "Invalid form data.", http.StatusBadRequest)
		log.Printf("Invalid form data: %v", r.Form)
		return
	}

	// Hash the password.
	hashedPassword, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	if err != nil {
		http.Error(w, "Failed to hash password.", http.StatusInternalServerError)
		log.Printf("Failed to hash password: %v", err)
		return
	}

	// Create a new user in BigQuery.
	query := bigqueryClient.Query("INSERT INTO `" + bigQueryDatasetID + "." + bigQueryTableID + "` (email, name, password) VALUES (@email, @name, @password)")
	query.Parameters = []bigquery.QueryParameter{
		{Name: "email", Value: email},
		{Name: "name", Value: name},
		{Name: "password", Value: hashedPassword},
	}
	_, err = query.Run(context.Background())
	if err != nil {
		http.Error(w, "Failed to insert user into BigQuery.", http.StatusInternalServerError)
		log.Printf("Failed to insert user into BigQuery: %v", err)
		return
	}

	// Send a message to the Pub/Sub topic.
	message := Message{Data: []byte(email)}
	messageJSON, err := json.Marshal(message)
	if err != nil {
		http.Error(w, "Failed to marshal message JSON.", http.StatusInternalServerError)
		log.Printf("Failed to marshal message JSON: %v", err)
		return
	}
	topic := pubSubClient.Topic(pubSubTopicID)
	result := topic.Publish(context.Background(), &pubsub.Message{Data: messageJSON})
	_, err = result.Get(context.Background())
	if err != nil {
		http.Error(w, "Failed to publish message to Pub/Sub topic.", http.StatusInternalServerError)
		log.Printf("Failed to publish message to Pub/Sub topic