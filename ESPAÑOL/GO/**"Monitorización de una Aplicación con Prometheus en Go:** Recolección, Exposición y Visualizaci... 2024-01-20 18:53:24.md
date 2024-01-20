```go
package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/gorilla/mux"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	// Namespace para todas las métricas
	namespace = "mi_aplicacion"

	// Registrador es el registro donde se registran las métricas
	registrador = prometheus.NewRegistry()

	// Counter es una métrica que cuenta los eventos que ocurren
	peticionesTotales = prometheus.NewCounter(
		prometheus.CounterOpts{
			Namespace: namespace,
			Name:      "peticiones_totales",
			Help:      "El número total de peticiones recibidas",
		},
	)

	// Gauge es una métrica que mide valores actuales
	peticionesActuales = prometheus.NewGauge(
		prometheus.GaugeOpts{
			Namespace: namespace,
			Name:      "peticiones_actuales",
			Help:      "El número de peticiones actualmente en proceso",
		},
	)

	// Histogram es una métrica que mide la distribución de eventos
	tiempoPeticion = prometheus.NewHistogram(
		prometheus.HistogramOpts{
			Namespace: namespace,
			Name:      "tiempo_peticion",
			Help:      "El tiempo que tardan las peticiones en procesarse",
			Buckets:   []float64{0.1, 0.5, 1, 2, 5, 10},
		},
	)

	// Summary es una métrica que mide la distribución de eventos y permite calcular valores como la mediana, el percentil 95, etc.
	latenciaPeticion = prometheus.NewSummary(
		prometheus.SummaryOpts{
			Namespace: namespace,
			Name:      "latencia_peticion",
			Help:      "La latencia de las peticiones",
		},
	)

	// Multiplexador para manejar peticiones HTTP
	multiplexador = mux.NewRouter()

	// Mutex para sincronizar el acceso al número de peticiones actuales
	mutex sync.Mutex
)

func main() {
	// Registrar las métricas en el registrador
	registrador.MustRegister(peticionesTotales, peticionesActuales, tiempoPeticion, latenciaPeticion)

	// Crear el servidor HTTP
	servidor := &http.Server{
		Addr:    ":8080",
		Handler: multiplexador,
	}

	// Crear el canal para capturar las señales de sistema
	canalSenales := make(chan os.Signal, 1)

	// Escuchar las señales de sistema
	signal.Notify(canalSenales, syscall.SIGINT, syscall.SIGTERM)

	// Crear la goroutine para manejar las peticiones HTTP
	go func() {
		if err := servidor.ListenAndServe(); err != http.ErrServerClosed {
			log.Fatalf("Error al iniciar el servidor: %v", err)
		}
	}()

	// Crear la goroutine para manejar las métricas
	go func() {
		// Crear un recolector de métricas
		recolectores := prometheus.NewGatherers(registrador)

		// Crear un servidor HTTP para exponer las métricas
		mux := http.NewServeMux()
		mux.Handle("/metrics", promhttp.HandlerFor(recolectores, promhttp.HandlerOpts{}))

		// Iniciar el servidor HTTP para las métricas
		if err := http.ListenAndServe(":9100", mux); err != nil {
			log.Fatalf("Error al iniciar el servidor de métricas: %v", err)
		}
	}()

	// Esperar a que se reciba una señal de sistema
	<-canalSenales

	// Cerrar el servidor HTTP
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := servidor.Shutdown(ctx); err != nil {
		log.Fatalf("Error al cerrar el servidor: %v", err)
	}

	// Imprimir las métricas
	fmt.Println("Métricas:")
	fmt.Println(registrador.Gather())
}

// ManejadorHTTP es el manejador de peticiones HTTP
func ManejadorHTTP(w http.ResponseWriter, r *http.Request) {
	// Incrementar el contador de peticiones totales
	peticionesTotales.Inc()

	// Obtener el tiempo de inicio de la petición
	inicio := time.Now()

	// Bloquear el acceso al número de peticiones actuales
	mutex.Lock()

	// Incrementar el número de peticiones actuales
	peticionesActuales.Inc()

	// Desbloquear el acceso al número de peticiones actuales
	mutex.Unlock()

	// Procesar la petición
	// ...

	// Obtener el tiempo de finalización de la petición
	fin := time.Now()

	// Calcular la latencia de la petición
	latencia := fin.Sub(inicio)

	// Medir la latencia de la petición
	latenciaPeticion.Observe(float64(latencia.Milliseconds()))

	// Medir el tiempo que tarda la petición en procesarse
	tiempoPeticion.Observe(float64(latencia.Seconds()))

	// Decrementar el número de peticiones actuales
	mutex.Lock()
	peticionesActuales.Dec()
	mutex.Unlock()

	// Responder a la petición
	fmt.Fprintf(w, "Hola, mundo!")
}

```

Explicación:

* El código anterior es un ejemplo de aplicación en Go que utiliza Prometheus para recolectar métricas.
* El código crea un servidor HTTP que expone las métricas recolectadas a través del punto final `/metrics`.
* Las métricas recolectadas incluyen el número total de peticiones, el número de peticiones actuales, el tiempo que tardan las peticiones en procesarse y la latencia de las peticiones.
* El código también muestra cómo registrar las métricas en un registro y cómo usarlas para medir el rendimiento de la aplicación.
* El código utiliza el paquete `github.com/prometheus/client_golang/prometheus` para crear las métricas y el paquete `github.com/prometheus/client_golang/prometheus/promhttp` para exponer las métricas a través del punto final `/metrics`.