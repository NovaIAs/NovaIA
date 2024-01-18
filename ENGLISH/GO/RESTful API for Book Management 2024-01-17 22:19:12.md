```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/postgres"
)

// Book represents a book in the database
type Book struct {
	ID        uint   `json:"id"`
	Title     string `json:"title"`
	Author    string `json:"author"`
	Publisher string `json:"publisher"`
	Year      int    `json:"year"`
}

// BookHandler struct contains Database connection
type BookHandler struct {
	db *gorm.DB
}

// NewBookHandler establishes connection to the database
func NewBookHandler() (*BookHandler, error) {
	// Get the connection string from the environment variable
	connectionString := os.Getenv("DATABASE_URL")

	// Create a new GORM database connection
	db, err := gorm.Open("postgres", connectionString)
	if err != nil {
		return nil, err
	}

	// Create the table if it doesn't exist
	if !db.HasTable(&Book{}) {
		if err := db.CreateTable(&Book{}).Error; err != nil {
			return nil, err
		}
	}

	return &BookHandler{db}, nil
}

// GetBooks returns all the books in the database
func (h *BookHandler) GetBooks(w http.ResponseWriter, r *http.Request) {
	var books []Book

	if result := h.db.Find(&books); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	fmt.Fprintf(w, "%+v", books)
}

// GetBook returns a book by its ID
func (h *BookHandler) GetBook(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var book Book

	if result := h.db.First(&book, id); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	fmt.Fprintf(w, "%+v", book)
}

// CreateBook creates a new book in the database
func (h *BookHandler) CreateBook(w http.ResponseWriter, r *http.Request) {
	var book Book

	if err := json.NewDecoder(r.Body).Decode(&book); err != nil {
		http.Error(w, http.StatusText(400), 400)
		return
	}

	if result := h.db.Create(&book); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	fmt.Fprintf(w, "%+v", book)
}

// UpdateBook updates a book in the database
func (h *BookHandler) UpdateBook(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var book Book

	if result := h.db.First(&book, id); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	if err := json.NewDecoder(r.Body).Decode(&book); err != nil {
		http.Error(w, http.StatusText(400), 400)
		return
	}

	if result := h.db.Save(&book); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	fmt.Fprintf(w, "%+v", book)
}

// DeleteBook deletes a book from the database
func (h *BookHandler) DeleteBook(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var book Book

	if result := h.db.First(&book, id); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	if result := h.db.Delete(&book); result.Error != nil {
		http.Error(w, http.StatusText(500), 500)
		return
	}

	fmt.Fprintf(w, "%+v", book)
}

func main() {
	// Start the server
	router := mux.NewRouter()

	h, err := NewBookHandler()
	if err != nil {
		log.Fatal(err)
	}

	router.HandleFunc("/books", h.GetBooks).Methods("GET")
	router.HandleFunc("/books/{id}", h.GetBook).Methods("GET")
	router.HandleFunc("/books", h.CreateBook).Methods("POST")
	router.HandleFunc("/books/{id}", h.UpdateBook).Methods("PUT")
	router.HandleFunc("/books/{id}", h.DeleteBook).Methods("DELETE")

	srv := &http.Server{
		Handler:      router,
		Addr:         "0.0.0.0:8080",
		WriteTimeout: 15 * time.Second,
		ReadTimeout:  15 * time.Second,
	}

	log.Fatal(srv.ListenAndServe())
}
```

This code creates a REST API for managing books in a database using the GORM library. It includes GET, POST, PUT, and DELETE operations for books. The API uses a JSON-based payload for data transfer.

**Explanation:**

1. **Database Connection:**
   - The `NewBookHandler` function establishes a connection to a PostgreSQL database using the `gorm` library.


2. **Book Model:**
   - `Book` struct represents a book's title, author, publisher, and year.


3. **HTTP Handlers:**
   - `GetBooks` retrieves all books from the database.
   - `GetBook` gets a specific book by its ID.
   - `CreateBook` creates a new book in the database.
   - `UpdateBook` updates an existing book in the database.
   - `DeleteBook` deletes a book from the database.


4. **Routing:**
   - The code uses the `mux` library for routing.
   - Each HTTP handler is assigned to a specific route and HTTP method.


5. **Server Configuration:**
   - The `main` function sets up the HTTP server configuration.
   - It specifies the port, write timeout, and read timeout.


6. **Starting the Server:**
   - The server starts listening on the specified port.

This code is a comprehensive REST API example using GORM and Go's standard libraries. It demonstrates a simple CRUD (Create, Read, Update, Delete) API for books.