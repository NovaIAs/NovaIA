```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
)

// Producto es una estructura que representa un producto en el sistema.
type Producto struct {
	ID        int       `json:"id"`
	Nombre     string    `json:"nombre"`
	Precio     float64  `json:"precio"`
	FechaVenta time.Time `json:"fecha_venta"`
}

// Productos es una lista de productos.
var Productos []Producto

// main es la función principal del programa.
func main() {
	// Creamos un nuevo enrutador.
	router := mux.NewRouter()

	// Creamos una nueva ruta para la página principal.
	router.HandleFunc("/", Index).Methods("GET")

	// Creamos una nueva ruta para la página de productos.
	router.HandleFunc("/productos", GetProductos).Methods("GET")

	// Creamos una nueva ruta para la página de crear un producto.
	router.HandleFunc("/productos/create", CrearProducto).Methods("POST")

	// Creamos una nueva ruta para la página de editar un producto.
	router.HandleFunc("/productos/{id}/edit", EditarProducto).Methods("GET")

	// Creamos una nueva ruta para la página de actualizar un producto.
	router.HandleFunc("/productos/{id}/update", ActualizarProducto).Methods("POST")

	// Creamos una nueva ruta para la página de eliminar un producto.
	router.HandleFunc("/productos/{id}/delete", EliminarProducto).Methods("GET")

	// Iniciamos el servidor.
	fmt.Println("Servidor escuchando en el puerto 8080")
	log.Fatal(http.ListenAndServe(":8080", router))
}

// Index es el controlador de la página principal.
func Index(w http.ResponseWriter, r *http.Request) {
	// Renderizamos la página principal.
	fmt.Fprintf(w, "<h1>Página principal</h1>")
}

// GetProductos es el controlador de la página de productos.
func GetProductos(w http.ResponseWriter, r *http.Request) {
	// Obtenemos todos los productos.
	productos, err := ObtenerProductos()
	if err != nil {
		fmt.Errorf("Error al obtener los productos: %v", err)
		http.Error(w, "Error al obtener los productos", http.StatusInternalServerError)
		return
	}

	// Renderizamos la página de productos.
	fmt.Fprintf(w, "<h1>Productos</h1>")
	for _, producto := range productos {
		fmt.Fprintf(w, "<li>%s - $%.2f</li>", producto.Nombre, producto.Precio)
	}
}

// CrearProducto es el controlador de la página de crear un producto.
func CrearProducto(w http.ResponseWriter, r *http.Request) {
	// Obtenemos los datos del formulario.
	nombre := r.FormValue("nombre")
	precio := r.FormValue("precio")

	// Creamos un nuevo producto.
	producto := Producto{
		Nombre: nombre,
		Precio: precio,
	}

	// Guardamos el producto en la base de datos.
	err := CrearProducto(producto)
	if err != nil {
		fmt.Errorf("Error al guardar el producto: %v", err)
		http.Error(w, "Error al guardar el producto", http.StatusInternalServerError)
		return
	}

	// Redireccionamos a la página de productos.
	http.Redirect(w, r, "/productos", http.StatusSeeOther)
}

// EditarProducto es el controlador de la página de editar un producto.
func EditarProducto(w http.ResponseWriter, r *http.Request) {
	// Obtenemos el ID del producto.
	id := mux.Vars(r)["id"]

	// Obtenemos el producto de la base de datos.
	producto, err := ObtenerProducto(id)
	if err != nil {
		fmt.Errorf("Error al obtener el producto: %v", err)
		http.Error(w, "Error al obtener el producto", http.StatusInternalServerError)
		return
	}

	// Renderizamos la página de editar un producto.
	fmt.Fprintf(w, "<h1>Editar producto</h1>")
	fmt.Fprintf(w, "<form action='/productos/%s/update' method='POST'>", id)
	fmt.Fprintf(w, "<label for='nombre'>Nombre:</label>")
	fmt.Fprintf(w, "<input type='text' name='nombre' value='%s'>", producto.Nombre)
	fmt.Fprintf(w, "<br>")
	fmt.Fprintf(w, "<label for='precio'>Precio:</label>")
	fmt.Fprintf(w, "<input type='number' name='precio' value='%s'>", producto.Precio)
	fmt.Fprintf(w, "<br>")
	fmt.Fprintf(w, "<input type='submit' value='Guardar'>")
	fmt.Fprintf(w, "</form>")
}

// ActualizarProducto es el controlador de la página de actualizar un producto.
func ActualizarProducto(w http.ResponseWriter, r *http.Request) {
	// Obtenemos el ID del producto.
	id := mux.Vars(r)["id"]

	// Obtenemos los datos del formulario.
	nombre := r.FormValue("nombre")
	precio := r.FormValue("precio")

	// Actualizamos el producto en la base de datos.
	producto := Producto{
		ID:        id,
		Nombre:     nombre,
		Precio:     precio,
		FechaVenta: time.Now(),