```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"time"

	"cloud.google.com/go/firestore"
	"cloud.google.com/go/pubsub"
)

// Este código crea un servidor web que escucha en el puerto 8080. Cuando se realiza una solicitud al servidor, genera un número aleatorio y lo envía a un tema de Cloud Pub/Sub.

// También crea un suscriptor de Cloud Pub/Sub que escucha el mismo tema y registra los números aleatorios recibidos.

func main() {
	// Crear un cliente de Cloud Pub/Sub.
	client, err := pubsub.NewClient(context.Background(), "mi-proyecto")
	if err != nil {
		log.Fatalf("pubsub.NewClient: %v", err)
	}
	defer client.Close()

	// Crear un tema de Cloud Pub/Sub.
	topic := client.Topic("numeros-aleatorios")

	// Crear una suscripción de Cloud Pub/Sub.
	subscription := client.Subscription("numeros-aleatorios-suscripcion")
	if err := subscription.Receive(context.Background(), func(ctx context.Context, msg *pubsub.Message) {
		// Procesar el número aleatorio recibido.
		var num int
		if err := json.Unmarshal(msg.Data, &num); err != nil {
			log.Printf("json.Unmarshal: %v", err)
			msg.Nack()
			return
		}
		log.Printf("Número aleatorio recibido: %d", num)

		// Marcar el mensaje como procesado.
		msg.Ack()
	}); err != nil {
		log.Fatalf("subscription.Receive: %v", err)
	}

	// Crear un servidor web.
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Generar un número aleatorio.
		num := rand.Intn(100)

		// Enviar el número aleatorio al tema de Cloud Pub/Sub.
		data, err := json.Marshal(num)
		if err != nil {
			log.Printf("json.Marshal: %v", err)
			w.WriteHeader(500)
			return
		}
		if _, err := topic.Publish(context.Background(), &pubsub.Message{Data: data}); err != nil {
			log.Printf("topic.Publish: %v", err)
			w.WriteHeader(500)
			return
		}

		// Responder al cliente.
		w.Write([]byte(fmt.Sprintf("Número aleatorio enviado: %d", num)))
	})

	// Iniciar el servidor web.
	log.Fatal(http.ListenAndServe(":8080", nil))
}
```

Explicación del código:

* La función `main()` crea un servidor web que escucha en el puerto 8080.
* Cuando se realiza una solicitud al servidor, genera un número aleatorio y lo envía a un tema de Cloud Pub/Sub.
* También crea un suscriptor de Cloud Pub/Sub que escucha el mismo tema y registra los números aleatorios recibidos.
* El servidor web utiliza la biblioteca estándar de `http` para manejar las solicitudes entrantes.
* La función `http.HandleFunc()` registra un manejador para la ruta raíz ("/") del servidor web.
* El manejador de la ruta raíz genera un número aleatorio y lo envía al tema de Cloud Pub/Sub.
* El manejador de la ruta raíz también responde al cliente con el número aleatorio generado.
* El suscriptor de Cloud Pub/Sub utiliza la biblioteca estándar de `pubsub` para escuchar el tema de Cloud Pub/Sub.
* El suscriptor registra una función de devolución de llamada que se ejecuta cuando se recibe un mensaje en el tema de Cloud Pub/Sub.
* La función de devolución de llamada procesa el número aleatorio recibido y lo registra.
* La función de devolución de llamada también marca el mensaje como procesado para que no se vuelva a procesar.