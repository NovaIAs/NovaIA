```go
package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
)

// Config contains the configuration for the application.
type Config struct {
	S3Bucket       string
	S3Region       string
	S3AccessKeyID  string
	S3SecretAccessKey string
	Port           string
}

// App contains the application state.
type App struct {
	config          Config
	s3Client        *s3.S3
	s3Uploader      *s3manager.Uploader
	wg              sync.WaitGroup
	done            chan struct{}
	items           map[string]Item
	itemsMutex      sync.Mutex
	uploadMutex     sync.Mutex
	imageMagickPath string
}

// Item represents an item in the system.
type Item struct {
	ID          string
	Name        string
	Description string
	Image       string
}

// NewApp creates a new App instance.
func NewApp(config Config) (*App, error) {
	app := &App{
		config: config,
		items:  make(map[string]Item),
		done:   make(chan struct{}),
	}

	sess, err := session.NewSession(&aws.Config{
		Region: aws.String(config.S3Region),
		Credentials: credentials.NewStaticCredentials(
			config.S3AccessKeyID,
			config.S3SecretAccessKey,
			"",
		),
	})
	if err != nil {
		return nil, err
	}

	app.s3Client = s3.New(sess)
	app.s3Uploader = s3manager.NewUploader(sess)

	imageMagickPath, err := filepath.Abs("/usr/bin/convert")
	if err != nil {
		return nil, err
	}
	app.imageMagickPath = imageMagickPath

	return app, nil
}

// Start starts the application.
func (app *App) Start() error {
	// Start a goroutine to listen for shutdown signals.
	app.wg.Add(1)
	go func() {
		defer app.wg.Done()
		<-app.done
	}()

	// Create a new router.
	router := mux.NewRouter()

	// Define the routes.
	router.HandleFunc("/", app.IndexHandler).Methods("GET")
	router.HandleFunc("/items", app.ItemsHandler).Methods("GET")
	router.HandleFunc("/items", app.CreateItemHandler).Methods("POST")
	router.HandleFunc("/items/{id}", app.GetItemHandler).Methods("GET")
	router.HandleFunc("/items/{id}", app.UpdateItemHandler).Methods("PUT")
	router.HandleFunc("/items/{id}", app.DeleteItemHandler).Methods("DELETE")
	router.HandleFunc("/items/{id}/image", app.UploadImageHandler).Methods("POST")

	// Start the HTTP server.
	srv := &http.Server{
		Addr:    ":" + app.config.Port,
		Handler: router,
	}

	go func() {
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			fmt.Printf("Error starting HTTP server: %v", err)
		}
	}()

	return nil
}

// Stop stops the application.
func (app *App) Stop() {
	// Signal the done channel to stop the goroutine that is listening for shutdown signals.
	close(app.done)

	// Wait for all goroutines to finish.
	app.wg.Wait()
}

// IndexHandler is the handler for the / route.
func (app *App) IndexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, world!")
}

// ItemsHandler is the handler for the /items route.
func (app *App) ItemsHandler(w http.ResponseWriter, r *http.Request) {
	app.itemsMutex.Lock()
	defer app.itemsMutex.Unlock()

	var items []Item
	for _, item := range app.items {
		items = append(items, item)
	}

	jsonBytes, err := json.Marshal(items)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonBytes)
}

// CreateItemHandler is the handler for the /items route.
func (app *App) CreateItemHandler(w http.ResponseWriter, r *http.Request) {
	var item Item
	if err := json.NewDecoder(r.Body).Decode(&item); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	app.itemsMutex.Lock()
	defer app.itemsMutex.Unlock()

	item.ID = uuid.New().String()
	app.items[item.ID] = item

	jsonBytes, err := json.Marshal(item)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonBytes)
}

// GetItemHandler is the handler for the /items/{id} route.
func (app *App) GetItemHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	app.itemsMutex.Lock()
	defer app.itemsMutex.Unlock()

	item, ok := app.items[id]
	if !ok {
		http.Error(w, "Item not found", http.StatusNotFound)
		return
	}

	jsonBytes, err := json.Marshal(item)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonBytes)
}

// UpdateItemHandler is the handler for the /items/{id} route.
func (app *App) UpdateItemHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	app.itemsMutex.Lock()
	defer app.itemsMutex.Unlock()

	item, ok := app.items[id]
	if !ok {
		http.Error(w, "Item not found", http.StatusNotFound)
		return
	}

	if err := json.NewDecoder(r.Body).Decode(&item); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	app.items[id] = item

	jsonBytes, err := json.Marshal(item)
	if err != nil {
		http.Error(w, "Internal server error", http