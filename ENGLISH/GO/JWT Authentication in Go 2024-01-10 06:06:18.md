```go
package main

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math/big"
	"net/http"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

// Config is the configuration for the application.
type Config struct {
	Port         int           `json:"port"`
	DatabaseURL  string        `json:"database_url"`
	JWTSecretKey string        `json:"jwt_secret_key"`
	MaxRetries   int           `json:"max_retries"`
	CacheTTL     time.Duration `json:"cache_ttl"`
}

// App is the main application struct.
type App struct {
	config     Config
	db         *Database
	jwtManager *JWTManager
	cache      *Cache
}

// NewApp creates a new App instance.
func NewApp() *App {
	config := loadConfig()
	db := NewDatabase(config.DatabaseURL)
	jwtManager := NewJWTManager(config.JWTSecretKey)
	cache := NewCache(config.CacheTTL)
	return &App{config, db, jwtManager, cache}
}

// Run starts the application.
func (app *App) Run() {
	router := mux.NewRouter()
	router.HandleFunc("/api/users", app.createUser).Methods(http.MethodPost)
	router.HandleFunc("/api/users/{id}", app.getUser).Methods(http.MethodGet)
	router.HandleFunc("/api/users/{id}", app.updateUser).Methods(http.MethodPut)
	router.HandleFunc("/api/users/{id}", app.deleteUser).Methods(http.MethodDelete)
	router.HandleFunc("/api/auth", app.authenticate).Methods(http.MethodPost)
	router.HandleFunc("/api/protected", app.protected).Methods(http.MethodGet)
	http.ListenAndServe(fmt.Sprintf(":%d", app.config.Port), router)
}

// createUser creates a new user in the database.
func (app *App) createUser(w http.ResponseWriter, r *http.Request) {
	var user User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	if user.Username == "" || user.Password == "" {
		http.Error(w, "Username and password are required", http.StatusBadRequest)
		return
	}
	user.Password = hashPassword(user.Password)
	if err := app.db.CreateUser(&user); err != nil {
		http.Error(w, "Error creating user", http.StatusInternalServerError)
		return
	}
	json.NewEncoder(w).Encode(user)
}

// getUser gets a user from the database.
func (app *App) getUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid ID", http.StatusBadRequest)
		return
	}
	user, err := app.db.GetUser(id)
	if err != nil {
		http.Error(w, "Error getting user", http.StatusInternalServerError)
		return
	}
	json.NewEncoder(w).Encode(user)
}

// updateUser updates a user in the database.
func (app *App) updateUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid ID", http.StatusBadRequest)
		return
	}
	var user User
	err = json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	user.ID = id
	if user.Password != "" {
		user.Password = hashPassword(user.Password)
	}
	if err := app.db.UpdateUser(&user); err != nil {
		http.Error(w, "Error updating user", http.StatusInternalServerError)
		return
	}
	json.NewEncoder(w).Encode(user)
}

// deleteUser deletes a user from the database.
func (app *App) deleteUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		http.Error(w, "Invalid ID", http.StatusBadRequest)
		return
	}
	if err := app.db.DeleteUser(id); err != nil {
		http.Error(w, "Error deleting user", http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// authenticate authenticates a user and returns a JWT.
func (app *App) authenticate(w http.ResponseWriter, r *http.Request) {
	var credentials Credentials
	err := json.NewDecoder(r.Body).Decode(&credentials)
	if err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	user, err := app.db.GetUserByUsername(credentials.Username)
	if err != nil {
		http.Error(w, "Error getting user", http.StatusInternalServerError)
		return
	}
	if !checkPasswordHash(credentials.Password, user.Password) {
		http.Error(w, "Invalid credentials", http.StatusUnauthorized)
		return
	}
	token, err := app.jwtManager.GenerateToken(user.ID)
	if err != nil {
		http.Error(w, "Error generating token", http.StatusInternalServerError)
		return
	}
	json.NewEncoder(w).Encode(map[string]string{"token": token})
}

// protected is a protected route that requires a JWT to access.
func (app *App) protected(w http.ResponseWriter, r *http.Request) {
	token := r.Header.Get("Authorization")
	token = strings.TrimPrefix(token, "Bearer ")
	claims, err := app.jwtManager.VerifyToken(token)
	if err != nil {
		http.Error(w, "Invalid token", http.StatusUnauthorized)
		return
	}
	user, err := app.db.GetUser(claims.UserID)
	if err != nil {
		http.Error(w, "Error getting user", http.StatusInternalServerError)
		return
	}
	json.NewEncoder(w).Encode(user)
}

// loadConfig loads the configuration from the config.json file.
func loadConfig() Config {
	file, err := os.Open("config.json")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	data, err := ioutil.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}
	config := Config{}
	err = json.Unmarshal(data, &config)
	if err != nil {
		log.Fatal(err)
	}
	return config
}

// hashPassword hashes a password using SHA256.
func hashPassword(password string) string {
	hasher := sha256.New()
	hasher.Write([]byte(password))
	return base64.StdEncoding.EncodeToString(hasher.Sum(nil))
}

// checkPasswordHash checks if a password matches a hashed password.
func checkPasswordHash(password, hashedPassword string) bool {
	hasher := sha256.New()
	hasher.Write([]byte(password))
	return base64.StdEncoding.EncodeToString(hasher.Sum(nil)) == hashedPassword
}

// generateRandomString generates a random string of a given length.
func generateRandomString(n int) string {
	const letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	b := make([]byte, n)
	for i := range b {
		randInt, err := rand.Int(rand.Reader, big.NewInt(int64(len(letters))))
		if err != nil {
			log.Fatal(err)
		}
		b[i] = letters[randInt.Int64()]
	}
	return string(b