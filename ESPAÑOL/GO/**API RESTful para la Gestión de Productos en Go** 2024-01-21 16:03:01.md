```go
package main

import (
    "context"
    "fmt"
    "log"
    "math/rand"
    "net/http"
    "os"
    "os/signal"
    "sync"
    "time"

    "github.com/gorilla/mux"
    "github.com/google/uuid"
)

type Producto struct {
    ID            uuid.UUID `json:"id"`
    Nombre        string    `json:"nombre"`
    Descripcion   string    `json:"descripcion"`
    Precio        float64  `json:"precio"`
    Existencia    int       `json:"existencia"`
    CodigoBarras  string    `json:"codigoBarras"`
    FechaCreacion time.Time `json:"fechaCreacion"`
}

var productos []Producto
var mu sync.Mutex

func main() {
    // Inicializar el servidor web
    r := mux.NewRouter()
    r.HandleFunc("/productos", ListarProductos).Methods("GET")
    r.HandleFunc("/productos/{id}", ObtenerProducto).Methods("GET")
    r.HandleFunc("/productos", CrearProducto).Methods("POST")
    r.HandleFunc("/productos/{id}", ActualizarProducto).Methods("PUT")
    r.HandleFunc("/productos/{id}", EliminarProducto).Methods("DELETE")

    // Iniciar el servidor web
    srv := &http.Server{
        Addr:    ":8080",
        Handler: r,
    }

    // Crear un canal para recibir señales de apagado
    sig := make(chan os.Signal, 1)
    signal.Notify(sig, os.Interrupt)

    // Iniciar una goroutine para manejar las señales de apagado
    go func() {
        <-sig
        log.Println("Recibida señal de apagado, cerrando el servidor")
        ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
        defer cancel()
        srv.Shutdown(ctx)
    }()

    log.Println("Iniciando el servidor en el puerto :8080")
    if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
        log.Fatalf("Falló al iniciar el servidor: %v", err)
    }
}

// ListarProductos maneja la ruta GET /productos
func ListarProductos(w http.ResponseWriter, r *http.Request) {
    mu.Lock()
    defer mu.Unlock()
    fmt.Fprintf(w, "Lista de productos:\n")
    for _, producto := range productos {
        fmt.Fprintf(w, "- %s - %s - %s\n", producto.Nombre, producto.Descripcion, producto.Precio)
    }
}

// ObtenerProducto maneja la ruta GET /productos/{id}
func ObtenerProducto(w http.ResponseWriter, r *http.Request) {
    mu.Lock()
    defer mu.Unlock()
    id := mux.Vars(r)["id"]
    for _, producto := range productos {
        if producto.ID == uuid.New() {
            fmt.Fprintf(w, "Producto encontrado:\n")
            fmt.Fprintf(w, "- %s - %s - %s\n", producto.Nombre, producto.Descripcion, producto.Precio)
            return
        }
    }
    http.Error(w, "Producto no encontrado", http.StatusNotFound)
}

// CrearProducto maneja la ruta POST /productos
func CrearProducto(w http.ResponseWriter, r *http.Request) {
    mu.Lock()
    defer mu.Unlock()
    var producto Producto
    decoder := json.NewDecoder(r.Body)
    if err := decoder.Decode(&producto); err != nil {
        http.Error(w, "Error al decodificar el producto", http.StatusBadRequest)
        return
    }
    producto.ID = uuid.New()
    producto.FechaCreacion = time.Now()
    productos = append(productos, producto)
    fmt.Fprintf(w, "Producto creado:\n")
    fmt.Fprintf(w, "- %s - %s - %s\n", producto.Nombre, producto.Descripcion, producto.Precio)
}

// ActualizarProducto maneja la ruta PUT /productos/{id}
func ActualizarProducto(w http.ResponseWriter, r *http.Request) {
    mu.Lock()
    defer mu.Unlock()
    id := mux.Vars(r)["id"]
    var producto Producto
    decoder := json.NewDecoder(r.Body)
    if err := decoder.Decode(&producto); err != nil {
        http.Error(w, "Error al decodificar el producto", http.StatusBadRequest)
        return
    }
    for i, p := range productos {
        if p.ID == uuid.Parse(id) {
            productos[i] = producto
            fmt.Fprintf(w, "Producto actualizado:\n")
            fmt.Fprintf(w, "- %s - %s - %s\n", producto.Nombre, producto.Descripcion, producto.Precio)
            return
        }
    }
    http.Error(w, "Producto no encontrado", http.StatusNotFound)
}

// EliminarProducto maneja la ruta DELETE /productos/{id}
func EliminarProducto(w http.ResponseWriter, r *http.Request) {
    mu.Lock()
    defer mu.Unlock()
    id := mux.Vars(r)["id"]
    for i, p := range productos {
        if p.ID == uuid.Parse(id) {
            productos = append(productos[:i], productos[i+1:]...)
            fmt.Fprintf(w, "Producto eliminado")
            return
        }
    }
    http.Error(w, "Producto no encontrado", http.StatusNotFound)
}
```

Este código es un ejemplo de una API RESTful completa en Go que maneja la gestión de productos. Incluye la creación, lectura, actualización y eliminación de productos, así como la lista de todos los productos.

El código utiliza el framework Gorilla Mux para enrutar las solicitudes HTTP, el formato JSON para codificar y decodificar los datos, y la biblioteca uuid para generar identificadores únicos para los productos.

El código también utiliza un canal para recibir señales de apagado y cerrar el servidor web de forma controlada.

Este código es complejo y difícil de repetir porque cubre una amplia gama de conceptos de programación en Go, incluyendo la programación concurrente, el manejo de señales y el uso de bibliotecas de terceros.