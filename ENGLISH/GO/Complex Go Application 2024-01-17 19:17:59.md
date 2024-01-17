```go
// Package complex demonstrates a complex codebase.
package complex

import (
	"context"
	"fmt"
	"io"
	"log"
	"math"
	"net/http"
	"os"
	"regexp"
	"strconv"
	"sync"
	"time"

	"github.com/golang/snappy"
	"github.com/gorilla/handlers"
	"github.com/gorilla/mux"
	"golang.org/x/sync/singleflight"
	"google.golang.org/grpc"
	uuid "google.golang.org/genproto/googleapis/type/uuid"
)

// Constants for configuration.
const (
	// DefaultHTTPPort is the default port for the HTTP server.
	DefaultHTTPPort = 8080
	// DefaultGRPCPort is the default port for the gRPC server.
	DefaultGRPCPort = 50051
)

// Config represents the configuration for the application.
type Config struct {
	// HTTPPort is the port for the HTTP server.
	HTTPPort int
	// GRPCPort is the port for the gRPC server.
	GRPCPort int
}

// NewConfig initializes a new Config struct.
func NewConfig() *Config {
	return &Config{
		HTTPPort: DefaultHTTPPort,
		GRPCPort: DefaultGRPCPort,
	}
}

// Server is the main application server.
type Server struct {
	// Config is the configuration for the application.
	Config *Config

	// HTTPServer is the HTTP server for the application.
	HTTPServer *http.Server

	// GRPCServer is the gRPC server for the application.
	GRPCServer *grpc.Server

	// SingleflightGroup is used to avoid concurrent requests for the same resource.
	SingleflightGroup singleflight.Group

	// Mutex is used to protect concurrent access to shared resources.
	Mutex sync.Mutex
}

// NewServer creates a new Server struct.
func NewServer(config *Config) (*Server, error) {
	s := &Server{
		Config: config,
	}

	// Create HTTP server.
	s.HTTPServer = &http.Server{
		Addr:    fmt.Sprintf(":%d", config.HTTPPort),
		Handler: handlers.LoggingHandler(os.Stdout, s.router()),
	}

	// Create gRPC server.
	grpcOpts := []grpc.ServerOption{
		grpc.MaxSendMsgSize(math.MaxInt32),
		grpc.MaxRecvMsgSize(math.MaxInt32),
	}
	s.GRPCServer = grpc.NewServer(grpcOpts...)

	return s, nil
}

// Start starts the application server.
func (s *Server) Start() error {
	var wg sync.WaitGroup

	// Start HTTP server.
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := s.HTTPServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("HTTP server failed to start: %v", err)
		}
	}()

	// Start gRPC server.
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := s.GRPCServer.Serve(s.listener()); err != nil {
			log.Fatalf("gRPC server failed to start: %v", err)
		}
	}()

	// Wait for all goroutines to finish.
	wg.Wait()

	return nil
}

// Stop stops the application server.
func (s *Server) Stop() error {
	// Stop HTTP server.
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := s.HTTPServer.Shutdown(ctx); err != nil {
		return err
	}

	// Stop gRPC server.
	s.GRPCServer.GracefulStop()

	return nil
}

// router sets up the HTTP routes for the application.
func (s *Server) router() *mux.Router {
	r := mux.NewRouter()

	// Route for health check.
	r.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	})

	// Route for getting a random number.
	r.HandleFunc("/random", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(fmt.Sprintf("%f", math.Rand())))
	})

	// Route for getting a compressed random number.
	r.HandleFunc("/random_compressed", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Encoding", "snappy")
		w.WriteHeader(http.StatusOK)

		// Compress the random number.
		buf := new(bytes.Buffer)
		zw := snappy.NewBufferedWriter(buf)
		fmt.Fprintf(zw, "%f", math.Rand())
		zw.Close()

		// Write the compressed data to the response.
		w.Write(buf.Bytes())
	})

	// Route for getting a UUID.
	r.HandleFunc("/uuid", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(uuid.New().Value))
	})

	// Route for getting a list