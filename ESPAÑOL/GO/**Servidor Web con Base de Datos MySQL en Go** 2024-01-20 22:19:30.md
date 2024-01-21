```go
// Módulo principal.
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/joho/godotenv"
)

// Constantes.
const (
	PORT = "8080"
	DB_NAME = "mi_base_de_datos"
	DB_USER = "mi_usuario"
	DB_PASS = "mi_contraseña"
)

// Variables globales.
var (
	db        *sql.DB
	httpServer *http.Server
)

// Función principal.
func main() {
	// Carga de variables de entorno.
	if err := godotenv.Load(); err != nil {
		log.Fatal(err)
	}

	// Conexión a la base de datos.
	var err error
	db, err = sql.Open("mysql", fmt.Sprintf("%s:%s@/%s", DB_USER, DB_PASS, DB_NAME))
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	// Creación del servidor HTTP.
	httpServer = &http.Server{
		Addr:    ":" + PORT,
		Handler: newRouter(),
	}

	// Escucha en el puerto especificado.
	log.Printf("Servidor escuchando en el puerto %s", PORT)
	log.Fatal(httpServer.ListenAndServe())
}

// Función que crea el router.
func newRouter() *http.ServeMux {
	router := http.NewServeMux()

	// Definición de rutas.
	router.HandleFunc("/", homeHandler)
	router.HandleFunc("/users", usersHandler)
	router.HandleFunc("/users/", userHandler)

	return router
}

// Manejador de la ruta raíz.
func homeHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "Hola, mundo!")
}

// Manejador de la ruta de usuarios.
func usersHandler(w http.ResponseWriter, r *http.Request) {
	// Obtiene todos los usuarios de la base de datos.
	rows, err := db.Query("SELECT * FROM usuarios")
	if err != nil {
		http.Error(w, "Error al obtener los usuarios", http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	// Crea un slice para almacenar los usuarios.
	var users []User

	// Recorre las filas y añade los usuarios al slice.
	for rows.Next() {
		var user User
		if err := rows.Scan(&user.ID, &user.Name, &user.Email); err != nil {
			http.Error(w, "Error al leer la fila", http.StatusInternalServerError)
			return
		}
		users = append(users, user)
	}

	// Codifica los usuarios en formato JSON y los escribe en la respuesta HTTP.
	if err := json.NewEncoder(w).Encode(users); err != nil {
		http.Error(w, "Error al codificar los usuarios", http.StatusInternalServerError)
		return
	}
}

// Manejador de la ruta de un usuario concreto.
func userHandler(w http.ResponseWriter, r *http.Request) {
	// Obtiene el ID del usuario de la ruta.
	id := strings.TrimPrefix(r.URL.Path, "/users/")

	// Obtiene el usuario de la base de datos.
	row := db.QueryRow("SELECT * FROM usuarios WHERE id = ?", id)

	// Crea una variable para almacenar el usuario.
	var user User

	// Lee la fila y almacena el usuario en la variable.
	if err := row.Scan(&user.ID, &user.Name, &user.Email); err != nil {
		http.Error(w, "Error al obtener el usuario", http.StatusInternalServerError)
		return
	}

	// Codifica el usuario en formato JSON y lo escribe en la respuesta HTTP.
	if err := json.NewEncoder(w).Encode(user); err != nil {
		http.Error(w, "Error al codificar el usuario", http.StatusInternalServerError)
		return
	}
}

// Estructura que representa un usuario.
type User struct {
	ID    int64
	Name  string
	Email string
}
```

Explicación:

* Este código crea un servidor web sencillo que permite obtener información de una base de datos MySQL.
* Utiliza la biblioteca `godotenv` para cargar variables de entorno desde un archivo `.env`.
* Se utiliza la biblioteca `github.com/joho/godotenv` para cargar las variables de entorno desde un archivo `.env`.
* Utiliza la biblioteca `database/sql` para conectarse a la base de datos MySQL.
* Utiliza la biblioteca `net/http` para crear un servidor web.
* Define varias rutas HTTP y sus correspondientes manejadores.
* El manejador `homeHandler` simplemente imprime "Hola, mundo!" en la respuesta HTTP.
* El manejador `usersHandler` obtiene todos los usuarios de la base de datos y los codifica en formato JSON antes de escribirlos en la respuesta HTTP.
* El manejador `userHandler` obtiene un usuario específico de la base de datos en función de su ID y lo codifica en formato JSON antes de escribirlo en la respuesta HTTP.