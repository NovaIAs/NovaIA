```go
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/gorilla/mux"
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/mysql"
)

// Crear la estructura Producto
type Producto struct {
	ID         int       `json:"id"`
	Nombre     string    `json:"nombre"`
	Descripcion string    `json:"descripcion"`
	Precio      float64  `json:"precio"`
	FechaAlta  time.Time `json:"fecha_alta"`
}

// Crear la estructura Error
type Error struct {
	Mensaje string `json:"mensaje"`
}

// Crear la estructura Respuesta
type Respuesta struct {
	Productos []Producto `json:"productos"`
	Error      Error      `json:"error"`
}

// Crear la base de datos
var db, err = gorm.Open("mysql", "usuario:contraseña@tcp(localhost:3306)/base_de_datos?charset=utf8&parseTime=True&loc=Local")

// Verificar si hay errores de conexión a la base de datos
if err != nil {
	log.Fatal(err)
}

// Cerrar la base de datos cuando se termine de usar
defer db.Close()

// Crear la tabla Producto
db.AutoMigrate(&Producto{})

// Función para obtener todos los productos
func ObtenerTodosLosProductos(w http.ResponseWriter, r *http.Request) {
	// Crear una variable para almacenar los productos
	var productos []Producto

	// Obtener todos los productos de la base de datos
	db.Find(&productos)

	// Crear una variable para almacenar la respuesta
	var respuesta Respuesta

	// Agregar los productos a la variable respuesta
	respuesta.Productos = productos

	// Codificar la respuesta en formato JSON
	respuestaJSON, err := json.Marshal(respuesta)

	// Verificar si hay errores en la codificación JSON
	if err != nil {
		// Crear una variable para almacenar el error
		var error Error

		// Agregar el mensaje de error a la variable error
		error.Mensaje = "Error al codificar la respuesta en formato JSON"

		// Agregar el error a la variable respuesta
		respuesta.Error = error

		// Codificar la respuesta en formato JSON
		respuestaJSON, err = json.Marshal(respuesta)

		// Verificar si hay errores en la codificación JSON
		if err != nil {
			http.Error(w, "Error al codificar la respuesta en formato JSON", http.StatusInternalServerError)
			return
		}
	}

	// Escribir la respuesta en el cuerpo de la solicitud
	w.Write(respuestaJSON)
}

// Función para obtener un producto por su ID
func ObtenerProductoPorID(w http.ResponseWriter, r *http.Request) {
	// Obtener el ID del producto de la URL
	id := mux.Vars(r)["id"]

	// Convertir el ID a entero
	idInt, err := strconv.Atoi(id)

	// Verificar si hay errores en la conversión del ID
	if err != nil {
		// Crear una variable para almacenar el error
		var error Error

		// Agregar el mensaje de error a la variable error
		error.Mensaje = "Error al convertir el ID a entero"

		// Agregar el error a la variable respuesta
		respuesta.Error = error

		// Codificar la respuesta en formato JSON
		respuestaJSON, err := json.Marshal(respuesta)

		// Verificar si hay errores en la codificación JSON
		if err != nil {
			http.Error(w, "Error al codificar la respuesta en formato JSON", http.StatusInternalServerError)
			return
		}

		// Escribir la respuesta en el cuerpo de la solicitud
		w.Write(respuestaJSON)

		return
	}

	// Crear una variable para almacenar el producto
	var producto Producto

	// Obtener el producto de la base de datos por su ID
	db.First(&producto, idInt)

	// Crear una variable para almacenar la respuesta
	var respuesta Respuesta

	// Agregar el producto a la variable respuesta
	respuesta.Productos = []Producto{producto}

	// Codificar la respuesta en formato JSON
	respuestaJSON, err := json.Marshal(respuesta)

	// Verificar si hay errores en la codificación JSON
	if err != nil {
		// Crear una variable para almacenar el error
		var error Error

		// Agregar el mensaje de error a la variable error
		error.Mensaje = "Error al codificar la respuesta en formato JSON"

		// Agregar el error a la variable respuesta
		respuesta.Error = error

		// Codificar la respuesta en formato JSON
		respuestaJSON, err = json.Marshal(respuesta)

		// Verificar si hay errores en la codificación JSON
		if err != nil {
			http.Error(w, "Error al codificar la respuesta en formato JSON", http.StatusInternalServerError)
			return
		}
	}

	// Escribir la respuesta en el cuerpo de la solicitud
	w.Write(respuestaJSON)
}

// Función para crear un producto
func CrearProducto(w http.ResponseWriter, r *http.Request) {
	// Obtener el cuerpo de la solicitud
	body, err := ioutil.ReadAll(r.Body)

	// Verificar si hay errores en la lectura del cuerpo de la solicitud
	if err != nil {
		// Crear una variable para almacenar el error
		var error Error

		// Agregar el mensaje de error a la variable error
		error.Mensaje = "Error al leer el cuerpo de la solicitud"

		// Agregar el error a la variable respuesta
		respuesta.Error = error

		// Codificar la respuesta en formato JSON
		respuestaJSON, err := json.Marshal(respuesta)

		// Verificar si hay errores en la codificación JSON
		if err != nil {
			http.Error(w, "Error al codificar la respuesta en formato JSON", http.StatusInternalServerError)
			return
		}

		// Escribir la respuesta en el cuerpo de la solicitud
		w.Write(respuestaJSON)

		return
	}

	// Crear una variable para almacenar el producto
	var producto Producto

	// Decodificar el cuerpo de la solicitud en el producto
	err = json.Unmarshal(body, &producto)

	// Verificar si hay errores en la decodificación del cuerpo de la solicitud
	if err != nil {
		// Crear una variable para almacenar el error
		var error Error

		// Agregar el mensaje de error a la variable error
		error.Mensaje = "Error al decodificar el cuerpo de la solicitud en el producto"

		// Agregar el error a la variable respuesta
		respuesta.Error = error

		// Codificar la respuesta en formato JSON
		respuestaJSON, err := json.Marshal(respuesta)

		// Verificar si hay errores