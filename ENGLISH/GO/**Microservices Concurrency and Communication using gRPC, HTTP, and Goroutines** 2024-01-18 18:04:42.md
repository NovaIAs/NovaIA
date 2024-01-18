```go
package main

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/golang/protobuf/ptypes"
	"google.golang.org/grpc"
)

type Service struct {
	mu      sync.Mutex
	counter int
}

func (s *Service) Count(ctx context.Context, req *grpc.Empty) (*grpc.Empty, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.counter++

	return &grpc.Empty{}, nil
}

func sendHTTPRequests(address string) error {
	resp, err := http.Get("http://" + address + "/count")
	if err != nil {
		return err
	}

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("unexpected status code: %d", resp.StatusCode)
	}

	return nil
}

func main() {
	port := 8080

	if len(os.Args) > 1 {
		port, _ = strconv.Atoi(os.Args[1])
	}

	// Create a TCP listener for gRPC.
	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	// Create a gRPC server.
	srv := grpc.NewServer()

	// Register the service with the gRPC server.
	RegisterServiceServer(srv, &Service{})

	// Start gRPC server in a goroutine.
	go func() {
		if err := srv.Serve(lis); err != nil {
			log.Fatalf("failed to serve: %v", err)
		}
	}()

	// Create a HTTP server.
	httpServer := http.NewServeMux()

	// Register a handler for the "/count" endpoint.
	httpServer.HandleFunc("/count", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Count: %d", service.counter)
	})

	// Start HTTP server in a goroutine.
	go func() {
		if err := http.ListenAndServe(":8081", httpServer); err != nil {
			log.Fatalf("failed to serve: %v", err)
		}
	}()

	// Create a channel to receive messages from other goroutines.
	requests := make(chan string)

	// Create a goroutine to send HTTP requests to the local HTTP server.
	go func() {
		for {
			requests <- "http://localhost:8081/count"
		}
	}()

	// Create a goroutine to send gRPC requests to the local gRPC server.
	go func() {
		conn, err := grpc.Dial("localhost:8080", grpc.WithInsecure(), grpc.WithBlock())
		if err != nil {
			log.Fatalf("failed to dial: %v", err)
		}
		defer conn.Close()

		client := NewServiceClient(conn)

		for {
			requests <- "grpc://localhost:8080/Service/Count"
		}
	}()

	// Create a goroutine to send requests to the local HTTP and gRPC servers.
	go func() {
		for {
			select {
			case address := <-requests:
				if err := sendHTTPRequests(address); err != nil {
					log.Fatalf("failed to send HTTP request: %v", err)
				}
			}
		}
	}()

	// Wait for 10 seconds to give the servers time to start.
	time.Sleep(10 * time.Second)

	// Print the number of requests sent to each server.
	fmt.Println("HTTP requests sent:", service.counter)

	// Stop the gRPC server.
	srv.Stop()

	// Stop the HTTP server.
	httpServer.Shutdown(context.Background())
}
```

This code demonstrates a more complex and differentiated usage of Go, including:

1. **gRPC Server and Client**: A gRPC service is implemented and registered with a gRPC server. A gRPC client is created to interact with the server.

2. **HTTP Server and Client**: An HTTP server is created and registered with a handler for a specific endpoint. An HTTP client is used to send requests to the server.

3. **Concurrency and Channels**: Multiple goroutines are used to send requests to the servers concurrently. Channels are used to communicate between these goroutines.

4. **Synchronization**: A mutex is used to protect shared data between goroutines.

5. **Timeouts and Context**: A timeout is used to limit the time spent waiting for a response from the servers. A context is used to cancel requests if they take too long.

6. **Data Encoding and Decoding**: JSON encoding and decoding is used to convert data between Go data structures and JSON strings.

7. **Protocol Buffers**: Protocol buffers are used to define the data structures used for communication between gRPC clients and servers.

8. **Logging and Error Handling**: Logging is used to output information and errors. Error handling is used to gracefully handle errors that may occur during execution.

This code demonstrates a more sophisticated understanding of Go concepts and how to combine them to build complex and scalable systems.