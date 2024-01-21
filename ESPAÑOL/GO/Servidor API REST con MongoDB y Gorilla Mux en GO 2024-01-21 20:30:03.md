```go
// Importamos las librerías necesarias
package main

import (
	"context"
	"encoding/json"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/joho/godotenv"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// Creamos una estructura para almacenar los datos de los usuarios
type User struct {
	ID        string `json:"id"`
	Nombre    string `json:"nombre"`
	Apellido  string `json:"apellido"`
	Correo    string `json:"correo"`
	Contraseña string `json:"contraseña"`
}

// Creamos una variable global para almacenar el cliente de MongoDB
var client *mongo.Client

// Función principal
func main() {
	// Cargamos las variables de entorno
	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error al cargar las variables de entorno")
	}

	// Nos conectamos a MongoDB
	client, err = mongo.NewClient(options.Client().ApplyURI(os.Getenv("MONGO_URI")))
	if err != nil {
		log.Fatal("Error al conectar a MongoDB")
	}

	// Creamos el contexto
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	// Nos aseguramos de cerrar la conexión a MongoDB cuando terminemos
	defer func() {
		if err := client.Disconnect(ctx); err != nil {
			log.Fatal("Error al cerrar la conexión a MongoDB")
		}
	}()

	// Creamos el enrutador
	router := mux.NewRouter()

	// Definimos las rutas
	router.HandleFunc("/api/users", GetUsers).Methods(http.MethodGet)
	router.HandleFunc("/api/users/{id}", GetUser).Methods(http.MethodGet)
	router.HandleFunc("/api/users", CreateUser).Methods(http.MethodPost)
	router.HandleFunc("/api/users/{id}", UpdateUser).Methods(http.MethodPut)
	router.HandleFunc("/api/users/{id}", DeleteUser).Methods(http.MethodDelete)

	// Iniciamos el servidor
	log.Fatal(http.ListenAndServe(":8080", router))
}

// Función para obtener todos los usuarios
func GetUsers(w http.ResponseWriter, r *http.Request) {
	// Creamos el contexto
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	// Obtenemos la colección de usuarios
	collection := client.Database("test").Collection("users")

	// Obtenemos todos los usuarios
	cursor, err := collection.Find(ctx, bson.M{})
	if err != nil {
		http.Error(w, "Error al obtener los usuarios", http.StatusInternalServerError)
		return
	}

	// Decodificamos los usuarios
	var users []*User
	for cursor.Next(ctx) {
		var user User
		if err := cursor.Decode(&user); err != nil {
			http.Error(w, "Error al decodificar los usuarios", http.StatusInternalServerError)
			return
		}
		users = append(users, &user)
	}

	// Codificamos los usuarios en JSON
	json, err := json.Marshal(users)
	if err != nil {
		http.Error(w, "Error al codificar los usuarios en JSON", http.StatusInternalServerError)
		return
	}

	// Enviamos la respuesta
	w.Header().Set("Content-Type", "application/json")
	w.Write(json)
}

// Función para obtener un usuario por su ID
func GetUser(w http.ResponseWriter, r *http.Request) {
	// Creamos el contexto
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	// Obtenemos el ID del usuario
	id := mux.Vars(r)["id"]

	// Obtenemos la colección de usuarios
	collection := client.Database("test").Collection("users")

	// Obtenemos el usuario por su ID
	result := collection.FindOne(ctx, bson.M{"id": id})

	// Decodificamos el usuario
	var user User
	if err := result.Decode(&user); err != nil {
		http.Error(w, "Error al decodificar el usuario", http.StatusInternalServerError)
		return
	}

	// Codificamos el usuario en JSON
	json, err := json.Marshal(user)
	if err != nil {
		http.Error(w, "Error al codificar el usuario en JSON", http.StatusInternalServerError)
		return
	}

	// Enviamos la respuesta
	w.Header().Set("Content-Type", "application/json")
	w.Write(json)
}

// Función para crear un usuario
func CreateUser(w http.ResponseWriter, r *http.Request) {
	// Creamos el contexto
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	// Obtenemos el cuerpo de la petición
	var user User
	if err := json.NewDecoder(r.Body).Decode(&user); err != nil {