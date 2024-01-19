```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/mux"
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/mysql"
)

type User struct {
	ID        uint64 `gorm:"primary_key;auto_increment"`
	Nombre    string `gorm:"size:255;not null;unique"`
	Apellido  string `gorm:"size:255;not null"`
	Email     string `gorm:"size:100;not null;unique"`
	Password  string `gorm:"size:60;not null"`
	CreatedAt time.Time
	UpdatedAt time.Time
}

var db *gorm.DB

func main() {
	var err error

	// Conectar a la base de datos
	db, err = gorm.Open("mysql", "usuario:contraseña@tcp(127.0.0.1:3306)/nombre_de_la_base_de_datos?charset=utf8&parseTime=True&loc=Local")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	// Crear las tablas en la base de datos
	db.AutoMigrate(&User{})

	// Crear un router para definir las rutas de la API
	router := mux.NewRouter()

	// Definir la ruta para crear un nuevo usuario
	router.HandleFunc("/usuarios", CrearUsuario).Methods("POST")

	// Definir la ruta para obtener todos los usuarios
	router.HandleFunc("/usuarios", ObtenerTodosLosUsuarios).Methods("GET")

	// Definir la ruta para obtener un usuario por su ID
	router.HandleFunc("/usuarios/{id}", ObtenerUsuarioPorID).Methods("GET")

	// Definir la ruta para actualizar un usuario por su ID
	router.HandleFunc("/usuarios/{id}", ActualizarUsuario).Methods("PUT")

	// Definir la ruta para eliminar un usuario por su ID
	router.HandleFunc("/usuarios/{id}", EliminarUsuario).Methods("DELETE")

	// Iniciar el servidor HTTP
	log.Fatal(http.ListenAndServe(":8080", router))
}

// CrearUsuario crea un nuevo usuario en la base de datos
func CrearUsuario(w http.ResponseWriter, r *http.Request) {
	var user User

	// Decodificar el cuerpo de la petición en una estructura User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Validar los campos de la estructura User
	if err := user.Validate(); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Crear el usuario en la base de datos
	db.Create(&user)

	// Codificar el usuario en formato JSON y escribirlo en la respuesta
	json.NewEncoder(w).Encode(user)
}

// ObtenerTodosLosUsuarios obtiene todos los usuarios de la base de datos
func ObtenerTodosLosUsuarios(w http.ResponseWriter, r *http.Request) {
	var users []User

	// Obtener todos los usuarios de la base de datos
	db.Find(&users)

	// Codificar los usuarios en formato JSON y escribirlos en la respuesta
	json.NewEncoder(w).Encode(users)
}

// ObtenerUsuarioPorID obtiene un usuario por su ID de la base de datos
func ObtenerUsuarioPorID(w http.ResponseWriter, r *http.Request) {
	var user User

	// Obtener el ID del usuario de la ruta
	id := mux.Vars(r)["id"]

	// Obtener el usuario de la base de datos por su ID
	db.First(&user, id)

	// Si el usuario no existe, devolver un error
	if user.ID == 0 {
		http.Error(w, "Usuario no encontrado", http.StatusNotFound)
		return
	}

	// Codific