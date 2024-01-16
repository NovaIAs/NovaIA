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

// Database connection object, using a global variable to simplify the example.
var db *sql.DB

// Song struct to model a song in the database.
type Song struct {
	ID     int    `json:"id"`
	Name   string `json:"name"`
	Artist string `json:"artist"`
	Album  string `json:"album"`
}

// Songs is a slice of Song objects.
type Songs []Song

// GetSongs returns all songs from the database.
func GetSongs(w http.ResponseWriter, r *http.Request) {
	songs, err := db.Query("SELECT * FROM songs")
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
	defer songs.Close()

	var songsList Songs
	for songs.Next() {
		var song Song
		if err := songs.Scan(&song.ID, &song.Name, &song.Artist, &song.Album); err != nil {
			http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
			return
		}
		songsList = append(songsList, song)
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(songsList); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
}

// GetSong returns a single song from the database.
func GetSong(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	songID := params["id"]

	song, err := db.QueryRow("SELECT * FROM songs WHERE id = ?", songID).Scan()
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(song); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
}

// CreateSong creates a new song in the database.
func CreateSong(w http.ResponseWriter, r *http.Request) {
	var song Song
	if err := json.NewDecoder(r.Body).Decode(&song); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	result, err := db.Exec("INSERT INTO songs (name, artist, album) VALUES (?, ?, ?)", song.Name, song.Artist, song.Album)
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	songID, err := result.LastInsertId()
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(map[string]interface{}{"id": songID}); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
}

// UpdateSong updates a song in the database.
func UpdateSong(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	songID := params["id"]

	var song Song
	if err := json.NewDecoder(r.Body).Decode(&song); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	result, err := db.Exec("UPDATE songs SET name = ?, artist = ?, album = ? WHERE id = ?", song.Name, song.Artist, song.Album, songID)
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(map[string]interface{}{"rowsAffected": rowsAffected}); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
}

// DeleteSong deletes a song from the database.
func DeleteSong(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	songID := params["id"]

	result, err := db.Exec("DELETE FROM songs WHERE id = ?", songID)
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(map[string]interface{}{"rowsAffected": rowsAffected}); err != nil {
		http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
		return
	}
}

func main() {
	// Load environment variables from .env file.
	err := godotenv.Load(".env")
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	// Create a database connection pool.
	db, err = sql.Open("mysql", os.Getenv("DB_DSN"))
	if err != nil {
		log.Fatal("Error opening database connection")
	}
	defer db.Close()

	// Ping the database to test the connection.
	if err := db.Ping(); err != nil {
		log.Fatal("Error pinging database")
	}

	// Create a new router.
	router := mux.NewRouter()

	// Define the routes.
	router.HandleFunc("/songs", GetSongs).Methods("GET")
	router.HandleFunc("/songs/{id}", GetSong).Methods("GET")
	router.HandleFunc("/songs", CreateSong).Methods("POST")
	router.HandleFunc("/songs/{id}", Update