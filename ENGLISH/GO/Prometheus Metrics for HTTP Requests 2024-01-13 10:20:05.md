```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	// Metrics
	requestCounter = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Name: "http_requests_total",
			Help: "Count of all HTTP requests.",
		},
		[]string{"code", "method"},
	)
	requestDuration = promauto.NewHistogramVec(
		prometheus.HistogramOpts{
			Name:    "http_request_duration_seconds",
			Help:    "Duration of all HTTP requests.",
			Buckets: []float64{0.1, 0.25, 0.5, 1, 2.5, 5, 10},
		},
		[]string{"code", "method"},
	)

	// Mutex for synchronizing access to the metrics.
	metricsMutex sync.Mutex
)

func main() {
	// Create a new router.
	router := mux.NewRouter()

	// Register the Prometheus metrics endpoint.
	router.Handle("/metrics", promhttp.Handler())

	// Register a handler for the root route.
	router.HandleFunc("/", rootHandler)

	// Register a handler for the "/hello" route.
	router.HandleFunc("/hello", helloHandler)

	// Start the HTTP server.
	log.Fatal(http.ListenAndServe(":8080", router))
}

// rootHandler is the handler for the root route.
func rootHandler(w http.ResponseWriter, r *http.Request) {
	// Increment the request counter for the root route.
	metricsMutex.Lock()
	requestCounter.WithLabelValues("200", r.Method).Inc()
	metricsMutex.Unlock()

	// Start a timer to measure the duration of the request.
	start := time.Now()

	// Write a response to the client.
	fmt.Fprint(w, "Hello, world!")

	// Stop the timer and record the duration of the request.
	duration := time.Since(start)
	metricsMutex.Lock()
	requestDuration.WithLabelValues("200", r.Method).Observe(duration.Seconds())
	metricsMutex.Unlock()
}

// helloHandler is the handler for the "/hello" route.
func helloHandler(w http.ResponseWriter, r *http.Request) {
	// Increment the request counter for the "/hello" route.
	metricsMutex.Lock()
	requestCounter.WithLabelValues("200", r.Method).Inc()
	metricsMutex.Unlock()

	// Start a timer to measure the duration of the request.
	start := time.Now()

	// Get the name parameter from the request.
	name := r.URL.Query().Get("name")

	// Write a response to the client.
	fmt.Fprintf(w, "Hello, %s!", name)

	// Stop the timer and record the duration of the request.
	duration := time.Since(start)
	metricsMutex.Lock()
	requestDuration.WithLabelValues("200", r.Method).Observe(duration.Seconds())
	metricsMutex.Unlock()
}
```

This code creates a simple HTTP server that uses the Gorilla Mux router and the Prometheus client library to expose metrics about the HTTP requests that are made to the server.

The code defines two Prometheus metrics:

* `requestCounter`: A counter that counts the number of HTTP requests that are made to the server.
* `requestDuration`: A histogram that measures the duration of HTTP requests.

The code also defines a `metricsMutex` which is used to synchronize access to the metrics.

The code then registers the Prometheus metrics endpoint at `/metrics`.

The code also defines two handlers:

* `rootHandler`: The handler for the root route (`/`).
* `helloHandler`: The handler for the `/hello` route.

Both handlers increment the `requestCounter` metric and start a timer to measure the duration of the request.

The `rootHandler` handler simply writes a response to the client.

The `helloHandler` handler gets the `name` parameter from the request and writes a response to the client.

Both handlers stop the timer and record the duration of the request in the `requestDuration` metric.

Finally, the code starts the HTTP server on port 8080.