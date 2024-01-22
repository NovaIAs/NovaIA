```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
	"github.com/robfig/cron/v3"
)

// Estructura de datos para almacenar la información de los registros.
type Registro struct {
	ID        int       `json:"id"`
	Timestamp time.Time `json:"timestamp"`
	Mensaje   string    `json:"mensaje"`
}

// Lista de registros para almacenar los registros creados.
var registros []Registro

// Función principal del programa.
func main() {
	// Carga las variables de entorno del archivo .env.
	err := godotenv.Load(".env")
	if err != nil {
		log.Fatal("Error al cargar las variables de entorno:", err)
	}

	// Crea una nueva instancia del servidor Gin.
	r := gin.Default()

	// Define la ruta para la creación de registros.
	r.POST("/registros", crearRegistro)

	// Define la ruta para la obtención de todos los registros.
	r.GET("/registros", obtenerRegistros)

	// Define la ruta para la eliminación de todos los registros.
	r.DELETE("/registros", eliminarRegistros)

	// Crea un nuevo objeto cron para programar la ejecución de tareas periódicas.
	c := cron.New(cron.WithSeconds())

	// Programa una tarea para ejecutar cada hora, que limpiará los registros antiguos.
	_, err = c.AddFunc("@hourly", limpiarRegistros)
	if err != nil {
		log.Fatal("Error al programar la tarea:", err)
	}

	// Inicia el servidor Gin y lo escucha en el puerto especificado en la variable de entorno PORT.
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}
	r.Run(":" + port)

	// Inicia el objeto cron, que ejecutará las tareas programadas.
	c.Start()
}

// Función para crear un nuevo registro.
func crearRegistro(c *gin.Context) {
	// Obtiene el mensaje del cuerpo de la solicitud.
	var registro Registro
	if err := c.BindJSON(&registro); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Error al leer el cuerpo de la solicitud."})
		return
	}

	// Genera un nuevo ID para el registro.
	registro.ID = len(registros) + 1

	// Agrega el registro a la lista de registros.
	registros = append(registros, registro)

	// Responde con el registro creado.
	c.JSON(http.StatusCreated, registro)
}

// Función para obtener todos los registros.
func obtenerRegistros(c *gin.Context) {
	// Responde con la lista de registros.
	c.JSON(http.StatusOK, registros)
}

// Función para eliminar todos los registros.
func eliminarRegistros(c *gin.Context) {
	// Reinicia la lista de registros.
	registros = nil

	// Responde con un mensaje de éxito.
	c.JSON(http.StatusOK, gin.H{"mensaje": "Todos los registros han sido eliminados."})
}

// Función para limpiar los registros antiguos.
func limpiarRegistros() {
	// Obtiene la fecha límite para los registros antiguos.
	limite := time.Now().AddDate(0, -1, 0)

	// Filtra los registros antiguos de la lista de registros.
	registros = registros[:0]
	for _, registro := range registros {
		if registro.Timestamp.Before(limite) {
			registros = append(registros, registro)
		}
	}
}


```