```go
// Creación de un servidor web sencillo en Go

package main

import (
    "fmt"
    "log"
    "net/http"
)

// Función principal
func main() {
    // Creación de un multiplexador
    mux := http.NewServeMux()

    // Manejador para la ruta "/"
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hola, mundo!")
    })

    // Manejador para la ruta "/hola"
    mux.HandleFunc("/hola", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hola, ¿cómo estás?")
    })

    // Creación de un servidor
    server := &http.Server{
        Addr:    ":8080",
        Handler: mux,
    }

    // Inicio del servidor
    log.Println("Iniciando el servidor en el puerto 8080")
    if err := server.ListenAndServe(); err != nil {
        log.Fatal(err)
    }
}
```

Explicación del código:

1. ```package main```: Esta línea indica que este es el paquete principal del programa.

2. ```import```: Se importan las bibliotecas necesarias para el programa.

3. ```func main()```: Esta función es el punto de entrada del programa.

4. ```mux := http.NewServeMux()```: Se crea un nuevo multiplexador, que es un objeto que se utiliza para asociar rutas con sus manejadores correspondientes.

5. ```mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) { ... })```: Se agrega un manejador para la ruta "/". Este manejador es una función anónima que escribe el mensaje "Hola, mundo!" en la respuesta.

6. ```mux.HandleFunc("/hola", func(w http.ResponseWriter, r *http.Request) { ... })```: Se agrega otro manejador para la ruta "/hola". Este manejador es una función anónima que escribe el mensaje "Hola, ¿cómo estás?" en la respuesta.

7. ```server := &http.Server{Addr: ":8080", Handler: mux}```: Se crea un nuevo servidor HTTP. La propiedad Addr especifica el puerto en el que el servidor escuchará las solicitudes. La propiedad Handler especifica el multiplexador que se utilizará para manejar las solicitudes.

8. ```if err := server.ListenAndServe(); err != nil { ... }```: Se inicia el servidor. Si ocurre algún error, se registra en el registro y el programa termina.