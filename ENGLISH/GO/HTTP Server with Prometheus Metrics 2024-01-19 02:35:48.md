```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	requestsTotal = promauto.NewCounter(prometheus.CounterOpts{
		Name: "http_requests_total",
		Help: "Total number of HTTP requests received.",
	})
	requestsDuration = promauto.NewHistogram(prometheus.HistogramOpts{
		Name:    "http_requests_duration_seconds",
		Help:    "Duration of HTTP requests in seconds.",
		Buckets: []float64{0.1, 0.25, 0.5, 1, 2.5, 5, 10},
	})
)

func main() {
	router := mux.NewRouter()
	router.HandleFunc("/", indexHandler)
	router.HandleFunc("/metrics", promhttp.Handler())

	server := http.Server{
		Addr:           ":8080",
		Handler:        router,
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}

	log.Fatal(server.ListenAndServe())
}

func indexHandler(w http.ResponseWriter, r *http.Request) {
	requestsTotal.Inc()

	start := time.Now()
	defer func() {
		requestsDuration.Observe(time.Since(start).Seconds())
	}()

	fmt.Fprintf(w, "Hello, world!")
}
```

This code is a simple HTTP server written in Go that exposes a Prometheus endpoint at `/metrics`. The server also logs the number of requests it receives and the duration of each request.

The following is a detailed explanation of the code:

* The `package main` line tells the Go compiler that this is the main program file.
* The `import` lines import the necessary libraries.
* The `var` block defines a set of Prometheus metrics.
* The `main` function is the entry point for the program.
* The `router` variable is used to define the routes for the HTTP server.
* The `server` variable is used to configure the HTTP server.
* The `indexHandler` function is the handler for the root route (`/`).
* The `requestsTotal` and `requestsDuration` metrics are incremented and observed in the `indexHandler` function.
* The `fmt.Fprintf` line writes a message to the HTTP response.
* The `server.ListenAndServe()` line starts the HTTP server.

This code is complex because it uses a number of different libraries and techniques. However, it is also a good example of how to write a simple HTTP server in Go.