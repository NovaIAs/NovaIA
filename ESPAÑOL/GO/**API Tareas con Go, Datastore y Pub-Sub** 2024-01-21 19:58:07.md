```go
// Importación de las bibliotecas necesarias.
import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"cloud.google.com/go/datastore"
	"cloud.google.com/go/pubsub"
	"github.com/gorilla/mux"
	"github.com/joho/godotenv"
)

// Define la estructura de la entidad "Tarea" que se almacenará en Datastore.
type Tarea struct {
	ID         int64     `datastore:"__key__"`
	Titulo      string    `datastore:"titulo"`
	Descripcion string    `datastore:"descripcion"`
	CreadaEn    time.Time `datastore:"creada_en"`
	Completada  bool      `datastore:"completada"`
}

// Define la estructura de la entidad "Mensaje" que se enviará a Pub/Sub.
type Mensaje struct {
	TareaID int64  `json:"tarea_id"`
	Titulo  string `json:"titulo"`
}

// Función principal.
func main() {
	// Carga las variables de entorno desde el archivo ".env".
	if err := godotenv.Load(); err != nil {
		log.Fatal("Error al cargar las variables de entorno:", err)
	}

	// Crea un cliente de Datastore.
	datastoreClient, err := datastore.NewClient(context.Background(), os.Getenv("PROJECT_ID"))
	if err != nil {
		log.Fatal("Error al crear el cliente de Datastore:", err)
	}
	defer datastoreClient.Close()

	// Crea un cliente de Pub/Sub.
	pubsubClient, err := pubsub.NewClient(context.Background(), os.Getenv("PROJECT_ID"))
	if err != nil {
		log.Fatal("Error al crear el cliente de Pub/Sub:", err)
	}
	defer pubsubClient.Close()

	// Crea un tópico de Pub/Sub llamado "tareas".
	topic := pubsubClient.Topic("tareas")

	// Crea un router HTTP usando Gorilla Mux.
	router := mux.NewRouter()

	// Define las rutas HTTP para crear, obtener, actualizar y eliminar tareas.
	router.HandleFunc("/tareas", crearTareaHandler(datastoreClient, topic)).Methods("POST")
	router.HandleFunc("/tareas/{id}", obtenerTareaHandler(datastoreClient)).Methods("GET")
	router.HandleFunc("/tareas/{id}", actualizarTareaHandler(datastoreClient, topic)).Methods("PUT")
	router.HandleFunc("/tareas/{id}", eliminarTareaHandler(datastoreClient, topic)).Methods("DELETE")

	// Inicia el servidor HTTP en el puerto 8080.
	log.Fatal(http.ListenAndServe(":8080", router))
}

// Función que maneja la solicitud POST para crear una nueva tarea.
func crearTareaHandler(datastoreClient *datastore.Client, topic *pubsub.Topic) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// Decodifica el cuerpo de la solicitud en una estructura "Tarea".
		var tarea Tarea
		if err := json.NewDecoder(r.Body).Decode(&tarea); err != nil {
			http.Error(w, "Error al decodificar el cuerpo de la solicitud:", http.StatusBadRequest)
			return
		}

		// Genera una clave única para la tarea.
		key := datastore.IncompleteKey("Tarea", nil)

		// Guarda la tarea en Datastore.
		if _, err := datastoreClient.Put(context.Background(), key, &tarea); err != nil {
			http.Error(w, "Error al guardar la tarea en Datastore:", http.StatusInternalServerError)
			return
		}

		// Crea un mensaje que contiene el ID de la tarea y su título.
		mensaje := Mensaje{
			TareaID: tarea.ID,
			Titulo:  tarea.Titulo,
		}

		// Publica el mensaje en el tópico "tareas".
		if _, err := topic.Publish(context.Background(), &pubsub.Message{
			Data: []byte(mensaje.JSON()),
		}).Get(context.Background()); err != nil {
			log.Println("Error al publicar el mensaje en Pub/Sub:", err)
		}

		// Envía una respuesta HTTP con el código de estado 201 Created y el ID de la tarea en el cuerpo de la respuesta.
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusCreated)
		if err := json.NewEncoder(w).Encode(tarea.ID); err != nil {
			http.Error(w, "Error al codificar la respuesta HTTP:", http.StatusInternalServerError)
		}
	}
}

// Función que maneja la solicitud GET para obtener una tarea por su ID.
func obtenerTareaHandler(datastoreClient *datastore.Client) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// Obtiene el ID de la tarea de los parámetros de la ruta.
		id := mux.Vars(r)["id"]

		// Genera una clave para la tarea.
		key := datastore.NameKey("Tarea", id, nil)

		// Obtiene la tarea de Datastore.
		var tarea Tarea
		if err := datastoreClient.Get(context.Background(), key, &tarea); err != nil {
			http.Error(w, "Error al obtener la tarea de Datastore:", http.StatusNotFound)
			return
		}

		// Envía una respuesta HTTP con el código de estado 200 OK y la tarea en el cuerpo de la respuesta.
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		if err := json.NewEncoder(w).Encode(tarea); err != nil {
			http.Error(w, "Error al codificar la respuesta HTTP:", http.StatusInternalServerError)
		}
	}
}

// Función que maneja la solicitud PUT para actualizar una tarea.
func actualizarTareaHandler(datastoreClient *datastore.Client, topic *pubsub.Topic) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// Obtiene el ID de la tarea de los parámetros de la ruta.
		id := mux.Vars(r)["id"]

		// Genera una clave para la tarea.
		key := datastore.NameKey("Tarea", id, nil)

		// Decodifica el cuerpo de la solicitud en una estructura "Tarea".
		var tarea Tarea
		if err := json.NewDecoder(r.Body).Decode(&tarea); err != nil {
			http.Error(w, "Error al decodificar el cuerpo de la solicitud:", http.StatusBadRequest)
			return
		}

		// Actualiza la tarea en Datastore.
		if _, err := datastoreClient.Put(context.Background(), key, &tarea); err != nil {
			http.Error(w, "Error al actualizar la tarea en Datastore:", http.StatusInternalServerError)
			return