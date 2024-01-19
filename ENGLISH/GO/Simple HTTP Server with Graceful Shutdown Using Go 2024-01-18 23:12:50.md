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
	"github.com/joho/godotenv"
)

// ServerConfig holds the configuration for the HTTP server.
type ServerConfig struct {
	Port    int
	Timeout time.Duration
}

// Server represents an HTTP server.
type Server struct {
	config *ServerConfig
	router *mux.Router
	srv    *http.Server
}

// NewServer creates a new HTTP server.
func NewServer(config *ServerConfig) *Server {
	return &Server{
		config: config,
		router: mux.NewRouter(),
	}
}

// Start starts the HTTP server.
func (s *Server) Start() error {
	s.srv = &http.Server{
		Addr:         fmt.Sprintf(":%d", s.config.Port),
		Handler:      s.router,
		ReadTimeout:  s.config.Timeout,
		WriteTimeout: s.config.Timeout,
	}

	// Handle graceful shutdown.
	go func() {
		quit := make(chan os.Signal)
		signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
		<-quit

		ctx, cancel := context.WithTimeout(context.Background(), s.config.Timeout)
		defer cancel()
		if err := s.srv.Shutdown(ctx); err != nil {
			log.Fatal(err)
		}
	}()

	return s.srv.ListenAndServe()
}

// RegisterRoutes registers the HTTP routes for the server.
func (s *Server) RegisterRoutes() {
	s.router.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, World!")
	})
}

// main is the entry point of the application.
func main() {
	// Load environment variables.
	if err := godotenv.Load(); err != nil {
		log.Fatal(err)
	}

	// Get the server configuration from the environment.
	port := os.Getenv("PORT")
	timeout := os.Getenv("TIMEOUT")

	// Validate the server configuration.
	config, err := ParseServerConfig(port, timeout)
	if err != nil {
		log.Fatal(err)
	}

	// Create a new server.
	server := NewServer(config)

	// Register the HTTP routes for the server.
	server.RegisterRoutes()

	// Start the server.
	if err := server.Start(); err != nil {
		log.Fatal(err)
	}
}

// ParseServerConfig parses the server configuration from the environment.
func ParseServerConfig(port, timeout string) (*ServerConfig, error) {
	var err error

	// Parse the port.
	config := &ServerConfig{}
	config.Port, err = strconv.Atoi(port)
	if err != nil {
		return nil, fmt.Errorf("invalid port: %w", err)
	}

	// Parse the timeout.
	config.Timeout, err = time.ParseDuration(timeout)
	if err != nil {
		return nil, fmt.Errorf("invalid timeout: %w", err)
	}

	return config, nil
}
```

This code creates a simple HTTP server using the Gorilla Mux router. The server listens on a specified port and responds to HTTP GET requests on the root path with the message "Hello, World!". The server also handles graceful shutdown when it receives a SIGINT or SIGTERM signal.

Here is an explanation of the code:

* The `ServerConfig` struct holds the configuration for the HTTP server, including the port and timeout.
* The `Server` struct represents an HTTP server and contains the server configuration, router, and HTTP server instance.
* The `NewServer` function creates a new HTTP server instance.
* The `Start` method starts the HTTP server and handles graceful shutdown.
* The `RegisterRoutes` method registers the HTTP routes for the server.
* The `main` function loads environment variables, parses the server configuration, creates a new server, registers the HTTP routes, and starts the server.
* The `ParseServerConfig` function parses the server configuration from the environment.

This code is complex because it handles different aspects of a web server, including configuration, routing, and graceful shutdown. It also involves working with environment variables, timeouts, and error handling. The code is also well-structured and organized, making it easier to read and maintain.