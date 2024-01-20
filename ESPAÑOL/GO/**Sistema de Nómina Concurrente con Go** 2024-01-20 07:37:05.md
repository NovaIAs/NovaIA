```go
// Paquete principal de la aplicación
package main

// Importar las bibliotecas necesarias
import (
	"fmt"
	"math"
	"net/http"
	"sync"
	"time"
)

// Definir una estructura para representar un empleado
type empleado struct {
	nombre string
	edad   int
	salario float64
}

// Definir una lista de empleados
var empleados = []empleado{
	{"Juan", 25, 1000000},
	{"María", 30, 1500000},
	{"Pedro", 35, 2000000},
}

// Definir un canal para enviar mensajes de error
var errores chan error

// Función principal de la aplicación
func main() {
	// Crear un grupo de goroutines
	var wg sync.WaitGroup
	
	// Recorrer la lista de empleados
	for _, empleado := range empleados {
		// Añadir una goroutine al grupo
		wg.Add(1)
		
		// Crear una goroutine para cada empleado
		go func(empleado empleado) {
			// Intentar obtener el valor del salario del empleado
			salario, err := obtenerSalario(empleado.nombre)
			
			// Enviar el error al canal de errores si se produce uno
			if err != nil {
				errores <- err
			}
			
			// Mostrar el salario del empleado
			fmt.Printf("El salario de %s es %f\n", empleado.nombre, salario)
			
			// Disminuir el contador del grupo de goroutines
			wg.Done()
		}(empleado)
	}
	
	// Esperar a que todas las goroutines terminen
	wg.Wait()
	
	// Recibir los errores del canal de errores
	for err := range errores {
		// Mostrar los errores
		fmt.Println(err)
	}
	
	// Iniciar un servidor web
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Mostrar el mensaje de bienvenida
		fmt.Fprintf(w, "Bienvenido al servidor web!")
	})
	
	// Iniciar el servidor web en el puerto 8080
	http.ListenAndServe(":8080", nil)
}

// Función para obtener el salario de un empleado
func obtenerSalario(nombre string) (float64, error) {
	// Simular una llamada a la base de datos
	time.Sleep(500 * time.Millisecond)
	
	// Comprobar si el empleado existe
	for _, empleado := range empleados {
		if empleado.nombre == nombre {
			return empleado.salario, nil
		}
	}
	
	// Devolver un error si el empleado no existe
	return 0, fmt.Errorf("El empleado %s no existe", nombre)
}
```

Explicación del código:

* El código crea una estructura `empleado` para representar la información de cada empleado.
* Define una lista de empleados con tres empleados ficticios.
* Crea un canal llamado `errores` para enviar mensajes de error.
* Crea un grupo de goroutines utilizando la función `sync.WaitGroup`.
* Recorre la lista de empleados y crea una goroutine para cada empleado, que se encarga de obtener el salario del empleado y mostrarlo.
* Utiliza la función `obtenerSalario` para obtener el salario de cada empleado. Esta función simula una llamada a la base de datos.
* Si se produce un error al obtener el salario, se envía el error al canal de errores.
* Utiliza la función `wg.Wait()` para esperar a que todas las goroutines terminen.
* Recibe los errores del canal de errores y los muestra.
* Inicia un servidor web que muestra un mensaje de bienvenida.
* Utiliza la función `http.HandleFunc` para manejar las solicitudes HTTP.
* Utiliza la función `http.ListenAndServe` para iniciar el servidor web en el puerto 8080.