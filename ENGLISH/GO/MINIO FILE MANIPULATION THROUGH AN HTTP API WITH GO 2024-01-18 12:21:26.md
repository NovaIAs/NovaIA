```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"sync"
	"time"

	"github.com/gorilla/mux"
	"github.com/joho/godotenv"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
)

// Structure that represents a user
type User struct {
	ID        int    `json:"id"`
	FirstName string `json:"firstName"`
	LastName  string `json:"lastName"`
	Email     string `json:"email"`
	Password  string `json:"password"`
}

// Structure that represents a file
type File struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	Extension string `json:"extension"`
	Size      int    `json:"size"`
	Path      string `json:"path"`
	Uploader  int    `json:"uploader"`
}

// Structure that represents a file upload response
type FileUploadResponse struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	Extension string `json:"extension"`
	Size      int    `json:"size"`
	Path      string `json:"path"`
}

// Structure that represents a file download response
type FileDownloadResponse struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	Extension string `json:"extension"`
	Size      int    `json:"size"`
	Path      string `json:"path"`
	Data      string `json:"data"`
}

// Structure that represents a file deletion response
type FileDeletionResponse struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	Extension string `json:"extension"`
	Size      int    `json:"size"`
	Path      string `json:"path"`
	Deleted   bool   `json:"deleted"`
}

// Structure that represents a list of files response
type ListFilesResponse struct {
	Files []File `json:"files"`
}

// Structure that represents a list of users response
type ListUsersResponse struct {
	Users []User `json:"users"`
}

// Create a new user
func createUser(w http.ResponseWriter, r *http.Request) {
	var user User

	// Decode the request body into the user struct
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, "Failed to decode request body", http.StatusBadRequest)
		return
	}

	// Validate the user struct
	if user.FirstName == "" || user.LastName == "" || user.Email == "" || user.Password == "" {
		http.Error(w, "Invalid user data", http.StatusBadRequest)
		return
	}

	// Create a new user in the database
	db, err := getDatabaseConnection()
	if err != nil {
		http.Error(w, "Failed to connect to the database", http.StatusInternalServerError)
		return
	}

	stmt, err := db.Prepare("INSERT INTO `users` (`first_name`, `last_name`, `email`, `password`) VALUES (?,?,?,?)")
	if err != nil {
		http.Error(w, "Failed to prepare the SQL statement", http.StatusInternalServerError)
		return
	}

	res, err := stmt.Exec(user.FirstName, user.LastName, user.Email, user.Password)
	if err != nil {
		http.Error(