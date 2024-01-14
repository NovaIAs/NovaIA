```go
package main

import (
	"context"
	"fmt"
	"html/template"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/gorilla/mux"
	"github.com/spf13/cobra"
)

// Config represents the application configuration.
type Config struct {
	Port        string `json:"port"`
	ReadTimeout time.Duration `json:"read_timeout"`
	WriteTimeout time.Duration `json:"write_timeout"`
	GracePeriod time.Duration `json:"grace_period"`
}

// App represents the application.
type App struct {
	config *Config
	router *mux.Router
}

// NewApp creates a new application.
func NewApp(config *Config) *App {
	app := &App{
		config: config,
		router: mux.NewRouter(),
	}

	app.setupRoutes()

	return app
}

// setupRoutes sets up the application routes.
func (app *App) setupRoutes() {
	app.router.HandleFunc("/", app.handleIndex).Methods("GET")
	app.router.HandleFunc("/healthz", app.handleHealthz).Methods("GET")
}

// handleIndex handles the index route.
func (app *App) handleIndex(w http.ResponseWriter, r *http.Request) {
	tmpl, err := template.ParseFiles("templates/index.html")
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	if err := tmpl.Execute(w, nil); err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}
}

// handleHealthz handles the healthz route.
func (app *App) handleHealthz(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "OK")
}

// Run starts the application.
func (app *App) Run() error {
	srv := &http.Server{
		Addr:           app.config.Port,
		Handler:        app.router,
		ReadTimeout:    app.config.ReadTimeout,
		WriteTimeout:   app.config.WriteTimeout,
		GracePeriod:    app.config.GracePeriod,
		MaxHeaderBytes: 1 << 20,
	}

	// Create a channel to listen for signals.
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Start the server in a goroutine.
	go func() {
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()

	// Wait for a signal to shut down the server.
	sig := <-sigChan
	log.Printf("Received signal: %v", sig)

	// Create a context with a grace period.
	ctx, cancel := context.WithTimeout(context.Background(), app.config.GracePeriod)
	defer cancel()

	// Shutdown the server.
	if err := srv.Shutdown(ctx); err != nil {
		log.Fatal(err)
	}

	log.Print("Server shutdown complete")

	return nil
}

// NewConfig creates a new configuration from the environment variables.
func NewConfig() (*Config, error) {
	config := &Config{
		Port:        os.Getenv("PORT"),
		ReadTimeout: time.Second * 10,
		WriteTimeout: time.Second * 10,
		GracePeriod: time.Second * 15,
	}

	if config.Port == "" {
		return nil, fmt.Errorf("PORT environment variable is not set")
	}

	return config, nil
}

// main is the entry point for the application.
func main() {
	config, err := NewConfig()
	if err != nil {
		log.Fatal(err)
	}

	app := NewApp(config)

	if err := app.Run(); err != nil {
		log.Fatal(err)
	}
}
```

This code is a complete web application written in Go. It includes a configuration struct, an application struct, a router, and a main function. The application handles two routes: "/" and "/healthz". The "/" route renders an index template, and the "/healthz" route returns "OK".

The application also includes a graceful shutdown handler, which listens for SIGINT and SIGTERM signals and shuts down the server after a grace period.

The NewConfig() function creates a new configuration struct from the environment variables. The main() function creates a new application and starts it.

This code is complex because it includes many different components, such as a configuration struct, an application struct, a router, a graceful shutdown handler, and a main function. It also includes two different routes, each with its own handler function.

This code would be difficult to repeat because it includes many different components and is not very modular. It would be difficult to change one part of the code without affecting other parts.