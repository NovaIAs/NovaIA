```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	httpErrors = promauto.NewCounter(prometheus.CounterOpts{
		Name: "http_errors_total",
		Help: "Número total de errores HTTP servidos.",
	})

	httpRequests = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Name: "http_requests_total",
			Help: "Número total de solicitudes HTTP servidas.",
		}, []string{"code", "method"},
	)

	httpRequestDuration = promauto.NewHistogram(
		prometheus.HistogramOpts{
			Name:    "http_request_duration_seconds",
			Help:    "Duración de las solicitudes HTTP.",
			Buckets: []float64{.1, .2, .3, .4, .5, .6, .7, .8, .9, 1},
		},
	)
)

func main() {
	router := mux.NewRouter()
	router.HandleFunc("/", IndexHandler)
	router.HandleFunc("/metrics", promhttp.Handler())
	log.Printf("Iniciando servidor HTTP en el puerto %s", os.Getenv("PORT"))
	log.Fatal(http.ListenAndServe(":"+os.Getenv("PORT"), router))
}

func IndexHandler(w http.ResponseWriter, r *http.Request) {
	start := time.Now()
	err := fmt.Fprintf(w, "Hola, mundo!")
	if err != nil {
		log.Printf("Error al escribir en el escritor: %v", err)
		httpErrors.Inc()
	}
	status := fmt.Sprintf("%d", http.StatusOK)
	httpRequests.WithLabelValues(status, r.Method).Inc()
	httpRequestDuration.Observe(time.Since(start).Seconds())
}
```
Este código crea un servidor HTTP sencillo en Go que usa la biblioteca `github.com/prometheus/client_golang` para exportar métricas de Prometheus. El servidor tiene una ruta raíz simple que devuelve "Hola, mundo!" y una ruta `/metrics` que sirve las métricas exportadas.

El código utiliza las siguientes librerías:

* `github.com/gorilla/mux`: Un router de HTTP que hace fácil el manejo de rutas.
* `github.com/prometheus/client_golang/prometheus`: Biblioteca de Prometheus que proporciona un marco para la recolección y exportación de métricas.
* `github.com/prometheus/client_golang/prometheus/promauto`: Un subpaquete de `prometheus` que genera métricas automáticamente.

Las siguientes métricas se exportan por el servidor:

* `http_errors_total`: El número total de errores HTTP servidos.
* `http_requests_total`: El número total de solicitudes HTTP servidas, agrupadas por código de estado HTTP y método HTTP.
* `http_request_duration_seconds`: La duración de las solicitudes HTTP, en segundos.

Estas métricas se pueden consultar mediante una herramienta de monitoreo como Prometheus o Grafana.