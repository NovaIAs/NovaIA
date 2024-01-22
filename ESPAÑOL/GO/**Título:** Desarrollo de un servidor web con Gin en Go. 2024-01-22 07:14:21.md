```go
// Programa principal

// Se importan las bibliotecas necesarias
import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
	"github.com/urfave/cli"
)

// Carga las variables de entorno
func loadEnvVariables() {
	// Comprueba si existe un archivo .env
	if _, err := os.Stat(".env"); err == nil {
		// Carga el archivo .env
		if err := godotenv.Load(); err != nil {
			log.Fatalf("Error al cargar el archivo .env: %v", err)
		}
	}
}

// Lanza el servidor HTTP
func startServer() {
	// Crea una instancia del motor de Gin
	r := gin.Default()

	// Define la ruta raíz
	r.GET("/", func(c *gin.Context) {
		c.String(http.StatusOK, "Hola, mundo!")
	})

	// Define una ruta para obtener la hora actual
	r.GET("/hora", func(c *gin.Context) {
		t := time.Now()
		c.JSON(http.StatusOK, gin.H{
			"hora": t.Format("15:04:05"),
			"fecha": t.Format("02-01-2006"),
		})
	})

	// Define una ruta para obtener una imagen aleatoria
	r.GET("/imagen", func(c *gin.Context) {
		// Obtiene una lista de imágenes disponibles
		images, err := ioutil.ReadDir("./images")
		if err != nil {
			log.Fatalf("Error al obtener la lista de imágenes: %v", err)
		}

		// Selecciona una imagen aleatoria
		randNum := int(time.Now().UnixNano())
		image := images[randNum%len(images)].Name()

		// Lee la imagen del disco
		data, err := ioutil.ReadFile("./images/" + image)
		if err != nil {
			log.Fatalf("Error al leer la imagen: %v", err)
		}

		// Envía la imagen al cliente
		c.Data(http.StatusOK, "image/png", data)
	})

	// Lanza el servidor
	r.Run()
}

// Función principal
func main() {
	// Carga las variables de entorno
	loadEnvVariables()

	// Crea una instancia de la aplicación CLI
	app := cli.NewApp()

	// Define las banderas de la aplicación
	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "port",
			Value: "8080",
			Usage: "Puerto en el que se ejecutará el servidor",
		},
	}

	// Define la acción de la aplicación
	app.Action = func(c *cli.Context) error {
		// Obtiene el puerto del servidor
		port := c.String("port")

		// Lanza el servidor
		startServer()

		return nil
	}

	// Ejecuta la aplicación
	if err := app.Run(os.Args); err != nil {
		log.Fatalf("Error al ejecutar la aplicación: %v", err)
	}
}
```

Este código crea un servidor web simple con Gin, que:

* Tiene una ruta raíz que devuelve "Hola, mundo!".
* Tiene una ruta para obtener la hora actual en formato JSON.
* Tiene una ruta para obtener una imagen aleatoria del directorio `images`.
* Se ejecuta en el puerto especificado por la bandera `--port`, o en el puerto 8080 por defecto.

El código también utiliza el paquete `godotenv` para cargar las variables de entorno desde un archivo `.env`.

Para utilizar este código, sigue estos pasos:

1. Crea un archivo `.env` en el directorio raíz del proyecto y añade las siguientes líneas:

```
PORT=8080
```

2. Instala las dependencias con el comando `go mod tidy`.

3. Ejecuta el programa con el comando `go run main.go`.

El servidor comenzará a ejecutarse en el puerto especificado en el archivo `.env`. Puedes acceder a las rutas del servidor visitando las siguientes direcciones:

* http://localhost:8080/
* http://localhost:8080/hora
* http://localhost:8080/imagen