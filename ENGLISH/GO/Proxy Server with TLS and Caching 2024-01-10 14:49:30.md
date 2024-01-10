```go
package main

import (
	"bufio"
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"hash/crc32"
	"io"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"regexp"
	"strings"
	"sync"
	"syscall"
	"time"
)

type Config struct {
	Port          int
	CertFile      string
	KeyFile       string
	UpstreamURL   string
	MaxConnections int
	Timeout        time.Duration
}

type Server struct {
	config Config

	listener net.Listener
	wg       sync.WaitGroup

	cache map[string]string
	mutex sync.Mutex
}

func NewServer(config Config) (*Server, error) {
	var err error

	// Check if the port is valid
	if config.Port <= 0 || config.Port > 65535 {
		return nil, fmt.Errorf("invalid port: %d", config.Port)
	}

	// Check if the certificate and key files exist
	if config.CertFile == "" || config.KeyFile == "" {
		return nil, fmt.Errorf("certificate and key files must be specified")
	}
	_, err = os.Stat(config.CertFile)
	if err != nil {
		return nil, fmt.Errorf("certificate file not found: %s", config.CertFile)
	}
	_, err = os.Stat(config.KeyFile)
	if err != nil {
		return nil, fmt.Errorf("key file not found: %s", config.KeyFile)
	}

	// Check if the upstream URL is valid
	if config.UpstreamURL == "" {
		return nil, fmt.Errorf("upstream URL must be specified")
	}
	_, err = url.Parse(config.UpstreamURL)
	if err != nil {
		return nil, fmt.Errorf("invalid upstream URL: %s", config.UpstreamURL)
	}

	// Check if the maximum number of connections is valid
	if config.MaxConnections <= 0 {
		return nil, fmt.Errorf("invalid maximum number of connections: %d", config.MaxConnections)
	}

	// Check if the timeout is valid
	if config.Timeout <= 0 {
		return nil, fmt.Errorf("invalid timeout: %s", config.Timeout)
	}

	// Create a new server
	server := &Server{
		config: config,
	}

	// Create a new cache
	server.cache = make(map[string]string)

	// Create a new listener
	server.listener, err = net.Listen("tcp", fmt.Sprintf(":%d", config.Port))
	if err != nil {
		return nil, fmt.Errorf("failed to listen on port %d: %s", config.Port, err)
	}

	// Start the server
	server.wg.Add(1)
	go server.start()

	return server, nil
}

func (s *Server) start() {
	// Handle signals
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Create a new TLS configuration
	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{
			{
				Certificate: [][]byte{
					s.config.CertFile,
				},
				PrivateKey: []byte(s.config.KeyFile),
			},
		},
	}

	// Start the HTTP server
	httpServer := &http.Server{
		Addr:      fmt.Sprintf(":%d", s.config.Port),
		TLSConfig: tlsConfig,
		Handler:   s,
	}

	// Wait for a signal to stop the server
	go func() {
		<-sigChan
		httpServer.Shutdown(context.Background())
		s.listener.Close()
		s.wg.Done()
	}()

	if err := httpServer.Serve(s.listener); err != nil && err != http.ErrServerClosed {
		log.Fatalf("failed to start HTTP server: %s", err)
	}
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	// Check if the request is valid
	if r.Method != "GET" {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Check if the request is authorized
	if r.Header.Get("Authorization") != "Bearer 123456" {
		http.Error(w, "unauthorized", http.StatusUnauthorized)
		return
	}

	// Get the request URL
	requestURL := r.URL.String()

	// Check if the request is cached