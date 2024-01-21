```go
// Paquete principal.
package main

// Bibliotecas importadas.
import (
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
)

// Crear un servidor web con un servidor HTTP.
var server = &http.Server{
	Addr: ":8080",
}

// Función principal.
func main() {
	// Crear un grupo de espera para esperar a que se completen las gorrutinas.
	var wg sync.WaitGroup

	// Función para manejar las solicitudes HTTP.
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		defer wg.Done()
		fmt.Fprintf(w, "Hola, mundo!")
	})

	// Iniciar el servidor web en una gorrutina independiente.
	go func() {
		if err := server.ListenAndServe(); err != nil {
			fmt.Println("Error al iniciar el servidor:", err)
			os.Exit(1)
		}
	}()

	// Crear un manejador de señales para SIGINT y SIGTERM.
	signalHandler := make(chan os.Signal, 1)
	signal.Notify(signalHandler, syscall.SIGINT, syscall.SIGTERM)

	// Esperar a que se reciba una señal.
	<-signalHandler

	// Detener el servidor web.
	if err := server.Shutdown(context.Background()); err != nil {
		fmt.Println("Error al detener el servidor:", err)
		os.Exit(1)
	}

	// Esperar a que todas las gorrutinas completen su ejecución.
	wg.Wait()
}
```

**Explicación**:

* El código anterior crea un servidor web con un servidor HTTP.
* La función `main()` crea un grupo de espera para esperar a que se completen las gorrutinas.
* La función `http.HandleFunc()` se utiliza para manejar las solicitudes HTTP.
* La gorrutina `go func() {}` se utiliza para iniciar el servidor web.
* El manejador de señales `signalHandler` se crea para manejar las señales `SIGINT` y `SIGTERM`.
* El código espera a que se reciba una señal en el canal `signalHandler`.
* Cuando se recibe una señal, el servidor web se detiene y se espera a que todas las gorrutinas completen su ejecución.

Este código es complejo porque utiliza varias características avanzadas del lenguaje Go, como gorrutinas y manejo de señales. También es complejo porque tiene una lógica compleja para manejar las solicitudes HTTP y detener el servidor web de forma segura.