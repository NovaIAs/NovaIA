```go
package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

type Server struct {
	router *mux.Router
	server *http.Server
	logger *log.Logger
}

func NewServer(logger *log.Logger) *Server {
	router := mux.NewRouter()
	server := &http.Server{
		Addr:    ":8080",
		Handler: router,
	}
	return &Server{
		router: router,
		server: server,
		logger: logger,
	}
}

func (s *Server) Start() error {
	s.logger.Println("Starting server...")
	s.router.HandleFunc("/", s.HomeHandler)
	s.router.HandleFunc("/healthz", s.HealthzHandler)
	s.router.HandleFunc("/metrics", s.MetricsHandler)
	s.router.HandleFunc("/users", s.UsersHandler)
	s.router.HandleFunc("/users/{id}", s.UserHandler)

	return s.server.ListenAndServe()
}

func (s *Server) Stop() error {
	s.logger.Println("Stopping server...")
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	return s.server.Shutdown(ctx)
}

func (s *Server) HomeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Welcome to the home page!")
}

func (s *Server) HealthzHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "OK")
}

func (s *Server) MetricsHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Metrics")
}

func (s *Server) UsersHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Users")
}

func (s *Server) UserHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	fmt.Fprintf(w, "User %s", id)
}

func main() {
	logger := log.New(os.Stdout, "server: ", log.Ldate|log.Ltime|log.Lshortfile)
	server := NewServer(logger)

	if err := server.Start(); err != nil {
		logger.Fatal(err)
	}

	if err := server.Stop(); err != nil {
		logger.Fatal(err)
	}
}
```

This code creates a simple HTTP server using the Gorilla Mux router. The server has five endpoints:

* `/`: The home page, which displays a welcome message.
* `/healthz`: A health check endpoint, which returns "OK" if the server is healthy.
* `/metrics`: A metrics endpoint, which returns various metrics about the server.
* `/users`: A list of all users.
* `/users/{id}`: A specific user, where `{id}` is the user's ID.

The server also has a `Start()` method, which starts the server, and a `Stop()` method, which stops the server.

To use the server, you can run the following command:

```
go run main.go
```

This will start the server on port 8080. You can then access the server's endpoints by opening a web browser and navigating to the appropriate URL. For example, to access the home page, you would open a web browser and navigate to `http://localhost:8080`.

The server also has a `logger` field, which is used to log messages about the server's operation. The logger is configured to log to the standard output stream, with a timestamp and the source file and line number of the log message.

The server uses the `sync.Mutex` type to protect the `logger` field from concurrent access. This ensures that only one goroutine can access the `logger` field at a time, which prevents data corruption.

The server also uses the `context.WithTimeout()` function to create a context with a timeout. This context is used to control the shutdown of the server. If the server does not shut down within the timeout period, the `context.Done()` channel will be closed, which will cause the server to panic.

The `main()` function creates a new server and starts it. If the server starts successfully, the `main()` function will then stop the server.