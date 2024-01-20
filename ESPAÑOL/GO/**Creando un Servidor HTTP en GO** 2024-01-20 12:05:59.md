```go
// Creando un Servidor HTTP en GO

// Importar el paquete "net/http"
package main

import (
    "fmt"
    "log"
    "net/http"
    "time"
)

// Función principal "main"
func main() {
    // Crear un nuevo servidor HTTP
    mux := http.NewServeMux()

    // Ruta "/", función anónima que escribe "Hola, Mundo!"
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hola, Mundo!")
    })

    // Ruta "/hora", función anónima que escribe la hora actual
    mux.HandleFunc("/hora", func(w http.ResponseWriter, r *http.Request) {
        t := time.Now()
        fmt.Fprintf(w, "La hora actual es: %v", t)
    })

    // Crear un servidor HTTP usando el multiplexor "mux"
    srv := &http.Server{
        Addr:    ":8080",
        Handler: mux,
    }

    // Iniciar el servidor y escuchar peticiones
    log.Fatal(srv.ListenAndServe())
}
```

**Explicación del código:**

* El código crea un nuevo servidor HTTP usando la biblioteca estándar de Go.
* Define dos rutas ("/" y "/hora") y asigna funciones anónimas para manejar las solicitudes a cada ruta.
* La ruta "/" simplemente escribe "Hola, Mundo!" en la respuesta.
* La ruta "/hora" utiliza el paquete "time" para obtener la hora actual y luego escribe la hora en la respuesta.
* El servidor HTTP se inicia usando el puerto 8080 y escucha las solicitudes.
* El código utiliza el registro estándar de Go para manejar cualquier error que ocurra durante la ejecución del servidor.