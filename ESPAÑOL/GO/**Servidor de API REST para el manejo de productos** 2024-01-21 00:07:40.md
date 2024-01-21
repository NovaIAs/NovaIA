```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/joho/godotenv"
)

// Crear una estructura para representar un producto.
type Producto struct {
	ID          int       `json:"id"`
	Nombre      string    `json:"nombre"`
	Descripcion string    `json:"descripcion"`
	Precio      float64  `json:"precio"`
	Imagen      string    `json:"imagen"`
	Creado      time.Time `json:"creado"`
	Actualizado time.Time `json:"actualizado"`
}

// Crear una lista de productos.
var productos []Producto = []Producto{
	{
		ID:          1,
		Nombre:      "iPhone 13 Pro Max",
		Descripcion: "El último y mejor smartphone de Apple",
		Precio:      999.99,
		Imagen:      "iphone-13-pro-max.jpg",
		Creado:      time.Now(),
		Actualizado: time.Now(),
	},
	{
		ID:          2,
		Nombre:      "Samsung Galaxy S22 Ultra",
		Descripcion: "El mejor smartphone Android del mercado",
		Precio:      1199.99,
		Imagen:      "samsung-galaxy-s22-ultra.jpg",
		Creado:      time.Now(),
		Actualizado: time.Now(),
	},
	{
		ID:          3,
		Nombre:      "Google Pixel 6 Pro",
		Descripcion: "El mejor smartphone con cámara del mercado",
		Precio:      899.99,
		Imagen:      "google-pixel-6-pro.jpg",
		Creado:      time.Now(),
		Actualizado: time.Now(),
	},
}

// Crear un manejador para obtener todos los productos.
func obtenerTodosLosProductos(w http.ResponseWriter, r *http.Request) {
	// Establecer el encabezado de Content-Type en application/json.
	w.Header().Set("Content-Type", "application/json")

	// Codificar la lista de productos en JSON y escribirla en la respuesta.
	json.NewEncoder(w).Encode(productos)
}

// Crear un manejador para obtener un producto por su ID.
func obtenerProductoPorID(w http.ResponseWriter, r *http.Request) {
	// Obtener el ID del producto de los parámetros de la ruta.
	vars := mux.Vars(r)
	id := vars["id"]

	// Buscar el producto en la lista de productos.
	var producto Producto
	encontrado := false
	for _, p := range productos {
		if p.ID == id {
			producto = p
			encontrado = true
			break
		}
	}

	// Si el producto no se encuentra, devolver un error 404.
	if !encontrado {
		http.Error(w, "Producto no encontrado", http.StatusNotFound)
		return
	}

	// Establecer el encabezado de Content-Type en application/json.
	w.Header().Set("Content-Type", "application/json")

	// Codificar el producto en JSON y escribirlo en la respuesta.
	json.NewEncoder(w).Encode(producto)
}

// Crear un manejador para crear un nuevo producto.
func crearProducto(w http.ResponseWriter, r *http.Request) {
	// Obtener el cuerpo de la solicitud.
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error al leer el cuerpo de la solicitud", http.StatusBadRequest)
		return
	}

	// Decodificar el cuerpo de la solicitud en un producto.
	var producto Producto
	if err := json.Unmarshal(body, &producto); err != nil {
		http.Error(w, "Error al decodificar el cuerpo de la solicitud", http.StatusBadRequest)
		return
	}

	// Asignar un nuevo ID al producto.
	producto.ID = len(productos) + 1

	// Añadir el producto a la lista de productos.
	productos = append(productos, producto)

	// Establecer el encabezado de Content-Type en application/json.
	w.Header().Set("Content-Type", "application/json")

	// Codificar el producto en JSON y escribirlo en la respuesta.
	json.NewEncoder(w).Encode(producto)
}

// Crear un manejador para actualizar un producto.
func actualizarProducto(w http.ResponseWriter, r *http.Request) {
	// Obtener el ID del producto de los parámetros de la ruta.
	vars := mux.Vars(r)
	id := vars["id"]

	// Obtener el cuerpo de la solicitud.
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error al leer el cuerpo de la solicitud", http.StatusBadRequest)
		return
	}

	// Decodificar el cuerpo de la solicitud en un producto.
	var producto Producto
	if err := json.Unmarshal(body, &producto); err != nil {
		http.Error(w, "Error al decodificar el cuerpo de la solicitud", http.StatusBadRequest)
		return
	}

	// Buscar el producto en la lista de productos.
	var encontrado bool
	for i, p := range productos {
		if p.ID == id {
			productos[i] = producto
			encontrado = true
			break
		}
	}

	// Si el producto no se encuentra, devolver un error 404.
	if !encontrado {
		http.Error(w, "Producto no encontrado", http.StatusNotFound)
		return
	}

	// Establecer el encabezado de Content-Type en application/json.
	w.Header().Set("Content-Type", "application/json")

	// Codificar el producto en JSON y escribirlo en la respuesta.
	json.NewEncoder(w).Encode(producto)
}

// Crear un manejador para eliminar un producto.
func eliminarProducto(w http.ResponseWriter, r *http.Request) {
	// Obtener el ID del producto de los parámetros de la ruta.
	vars := mux.Vars(r)
	id := vars["id"]

	// Buscar el producto en la lista de productos.
	var encontrado bool
	var index int
	for i, p := range productos {
		if p.ID == id {
			encontrado = true
			index = i
			break
		}
	}

	// Si el producto no se encuentra, devolver un error 404.
	if !encontrado {
		http.Error(w, "Producto no encontrado", http.StatusNotFound)
		return
	}

	// Eliminar el producto de la lista de productos.
	productos = append(productos[:index], productos[index+1:]...)

	// Establecer el encabezado de Content-Type en application/json.
	w.Header().Set("Content-Type", "application/json")

	// Codificar el producto en JSON y escribirlo en la respuesta.
	json.NewEncoder(w).Encode(map[string]string{"mensaje": "Producto eliminado"})
}

// Función principal.
func main() {
	// Cargar las variables de entorno.
	err := godotenv.Load(".env")
	if err != nil {
		log.Fatal("Error al cargar las variables de entorno")
	}

	// Crear un router.
	router := mux.NewRouter()

	// Definir las rutas.
	router.HandleFunc("/productos", obtenerTodosLosProductos).Methods("GET")
	router.HandleFunc("/productos/{id}", obtenerProductoPorID).Methods("GET")
	router.HandleFunc("/productos", crearProducto).Methods("POST")
	router.HandleFunc("/productos/{id}", actualizarProducto).Methods("PUT")
	router.HandleFunc("/productos/{id}", eliminarProducto).Methods("DELETE")

	// Obtener el puerto del servidor de la variable de entorno.
	port := os.Getenv("PORT")

	// Iniciar el servidor.
	log.Printf("El servidor se está ejecutando en el puerto %s", port)
	log.Fatal(http.ListenAndServe(":"+port, router))
}
```

Explicación del código:

* El código utiliza el framework Gorilla Mux para crear un router HTTP.
* El código define una estructura `Producto` para representar los productos.
* El código crea una lista de productos.
* El código define un conjunto de manejadores para obtener, crear, actualizar y eliminar productos.
* El código utiliza el paquete `ioutil` para leer el cuerpo de las solicitudes HTTP.
* El código utiliza el paquete `json` para codificar y decodificar los productos en JSON.
* El código utiliza el paquete `os` para obtener el puerto del servidor de la variable de entorno.
* El código utiliza el paquete `log` para registrar los mensajes del servidor.
* El código inicia el servidor en el puerto especificado en la variable de entorno `PORT`.