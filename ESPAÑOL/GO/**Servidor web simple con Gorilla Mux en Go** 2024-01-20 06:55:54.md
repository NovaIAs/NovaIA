```go
// Importamos las bibliotecas que vamos a utilizar.
import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
)

// Definimos una función que maneja las solicitudes GET a la raíz.
func main() {
	// Creamos un nuevo enrutador de Gorilla Mux.
	r := mux.NewRouter()

	// Añadimos una ruta para manejar las solicitudes GET a la raíz.
	r.HandleFunc("/", indexHandler)

	// Añadimos una ruta para manejar las solicitudes GET a /saludo.
	r.HandleFunc("/saludo", saludoHandler)

	// Añadimos una ruta para manejar las solicitudes GET a /hora.
	r.HandleFunc("/hora", horaHandler)

	// Iniciamos el servidor en el puerto 8080.
	log.Fatal(http.ListenAndServe(":8080", r))
}

// Definimos la función que maneja las solicitudes GET a la raíz.
func indexHandler(w http.ResponseWriter, r *http.Request) {
	// Escribimos un mensaje en la respuesta.
	fmt.Fprintf(w, "Hola, mundo!")
}

// Definimos la función que maneja las solicitudes GET a /saludo.
func saludoHandler(w http.ResponseWriter, r *http.Request) {
	// Obtenemos el nombre del parámetro "nombre" de la solicitud.
	nombre := r.URL.Query().Get("nombre")

	// Si el nombre no está definido, lo configuramos a "mundo".
	if nombre == "" {
		nombre = "mundo"
	}

	// Escribimos un mensaje en la respuesta.
	fmt.Fprintf(w, "Hola, %s!", nombre)
}

// Definimos la función que maneja las solicitudes GET a /hora.
func horaHandler(w http.ResponseWriter, r *http.Request) {
	// Obtenemos la hora actual.
	hora := time.Now()

	// Formateamos la hora a una cadena.
	horaFormateada := hora.Format("02/01/2006 15:04:05")

	// Escribimos un mensaje en la respuesta.
	fmt.Fprintf(w, "La hora actual es %s.", horaFormateada)
}
```


***Explicación:***

Este código crea un servidor web simple utilizando la biblioteca Gorilla Mux en Go. El servidor maneja tres solicitudes GET diferentes.

* La primera solicitud GET es a la raíz del sitio web (`/`). Cuando se realiza esta solicitud, el servidor responde con el mensaje "Hola, mundo!".
* La segunda solicitud GET es a la ruta `/saludo`. Esta solicitud tiene un parámetro opcional llamado `nombre`. Si el parámetro `nombre` no se proporciona, el servidor responde con el mensaje "Hola, mundo!". De lo contrario, el servidor responde con el mensaje "Hola, [nombre del parámetro]!".
* La tercera solicitud GET es a la ruta `/hora`. Cuando se realiza esta solicitud, el servidor responde con la hora actual formateada como una cadena.

Este es sólo un ejemplo simple de cómo se puede utilizar Gorilla Mux para crear un servidor web en Go. Hay muchas otras cosas que se pueden hacer con Gorilla Mux, como crear enrutadores más complejos, manejar solicitudes POST y PUT, y servir archivos estáticos.