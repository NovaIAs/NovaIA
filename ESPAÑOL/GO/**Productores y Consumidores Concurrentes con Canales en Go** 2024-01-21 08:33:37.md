```go
// Paquete principal
package main

// Importación de librerías necesarias
import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Definición del tipo Mensaje
type Mensaje struct {
	Id   int
	Dato string
}

// Definición del canal de mensajes
var canalMensajes = make(chan Mensaje)

// Función productora
func productor(idProductor int) {
	for i := 0; i < 10; i++ {
		mensaje := Mensaje{Id: idProductor, Dato: fmt.Sprintf("Dato %d de Productor %d", i, idProductor)}
		canalMensajes <- mensaje
		fmt.Printf("Productor %d: Envío el mensaje: %v\n", idProductor, mensaje)
		time.Sleep(100 * time.Millisecond)
	}
}

// Función consumidora
func consumidor(idConsumidor int) {
	for {
		mensaje := <-canalMensajes
		fmt.Printf("Consumidor %d: Recibí el mensaje: %v\n", idConsumidor, mensaje)
		time.Sleep(rand.Intn(500) * time.Millisecond)
	}
}

// Función principal
func main() {
	// Inicialización del generador de números aleatorios
	rand.Seed(time.Now().UnixNano())

	// Creación de los grupos de procesos para los productores y consumidores
	productores := sync.WaitGroup{}
	consumidores := sync.WaitGroup{}

	// Creación de los productores
	for i := 0; i < 3; i++ {
		productores.Add(1)
		go func(id int) {
			defer productores.Done()
			productor(id)
		}(i)
	}

	// Creación de los consumidores
	for i := 0; i < 5; i++ {
		consumidores.Add(1)
		go func(id int) {
			defer consumidores.Done()
			consumidor(id)
		}(i)
	}

	// Espera a que todos los productores y consumidores terminen su ejecución
	productores.Wait()
	consumidores.Wait()

	fmt.Println("Todos los mensajes han sido enviados y recibidos.")
}
```

**Explicación del código:**

* El código crea un canal de mensajes (canalMensajes) para comunicarse entre los productores y los consumidores.
* La función productora envía 10 mensajes al canal de mensajes. El mensaje contiene un código identificativo del productor y un dato.
* La función consumidora lee los mensajes del canal de mensajes e imprime el código identificativo del consumidor y el dato del mensaje.
* La función principal crea los grupos de procesos para los productores y consumidores. Los productores envían los mensajes al canal de mensajes y los consumidores los reciben del canal de mensajes.
* El código espera a que todos los productores y consumidores terminen su ejecución antes de imprimir el mensaje "Todos los mensajes han sido enviados y recibidos".