```go
package main

import (
	"bufio"
	"crypto/sha256"
	"encoding/base64"
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"strings"
	"sync"
	"time"
)

// Comando constante de la línea de comandos.
const (
	DefaultPort = 8080
	DefaultAddr = "localhost"
)

// Define las variables de la línea de comandos.
var (
	addr = flag.String("addr", DefaultAddr, "Dirección del servidor.")
	port = flag.Int("port", DefaultPort, "Puerto del servidor.")
)

// Estructura de datos para almacenar la información de los clientes conectados.
type Client struct {
	conn net.Conn
	name string
}

// Estructura de datos para almacenar los mensajes de chat.
type Message struct {
	sender string
	body   string
	time   time.Time
}

// Mapa para almacenar los clientes conectados.
var clients = make(map[string]*Client)

// Canal para almacenar los mensajes de chat.
var messages = make(chan Message)

// Mutex para proteger el acceso al mapa de clientes.
var mutex = &sync.RWMutex{}

// Función principal.
func main() {
	// Parsear las banderas de la línea de comandos.
	flag.Parse()

	// Crear un servidor TCP.
	ln, err := net.Listen("tcp", fmt.Sprintf("%s:%d", *addr, *port))
	if err != nil {
		log.Fatal(err)
	}
	defer ln.Close()

	// Ciclo infinito para aceptar conexiones de clientes.
	for {
		// Aceptar una conexión de cliente.
		conn, err := ln.Accept()
		if err != nil {
			log.Println(err)
			continue
		}

		// Crear un nuevo cliente.
		client := &Client{conn: conn}

		// Leer el nombre del cliente.
		client.name, err = bufio.NewReader(conn).ReadString('\n')
		if err != nil {
			log.Println(err)
			conn.Close()
			continue
		}

		// Eliminar los espacios en blanco del nombre del cliente.
		client.name = strings.TrimSpace(client.name)

		// Si el nombre del cliente está vacío, cerrarlo.
		if client.name == "" {
			conn.Close()
			continue
		}

		// Agregar el cliente al mapa de clientes.
		mutex.Lock()
		clients[client.name] = client
		mutex.Unlock()

		// Enviar un mensaje de bienvenida al cliente.
		conn.Write([]byte("Bienvenido al chat.\n"))

		// Iniciar una goroutine para manejar los mensajes del cliente.
		go handleClient(client)
	}
}

// Función para manejar los mensajes de un cliente.
func handleClient(client *Client) {
	// Ciclo infinito para leer los mensajes del cliente.
	for {
		// Leer un mensaje del cliente.
		msg, err := bufio.NewReader(client.conn).ReadString('\n')
		if err != nil {
			log.Println(err)
			break
		}

		// Eliminar los espacios en blanco del mensaje del cliente.
		msg = strings.TrimSpace(msg)

		// Si el mensaje del cliente está vacío, continuar.
		if msg == "" {
			continue
		}

		// Crear un nuevo mensaje.
		message := Message{sender: client.name, body: msg, time: time.Now()}

		// Enviar el mensaje al canal de mensajes.
		messages <- message
	}

	// Eliminar el cliente del mapa de clientes.
	mutex.Lock()
	delete(clients, client.name)
	mutex.Unlock()

	// Cerrar la conexión del cliente.
	client.conn.Close()
}

// Función para manejar los mensajes de chat.
func handleMessages() {
	// Ciclo infinito para leer los mensajes del canal de mensajes.
	for {
		// Leer un mensaje del canal de mensajes.
		message := <-messages

		// Generar un hash SHA256 del mensaje.
		hash := sha256.Sum256([]byte(message.sender + message.body))

		// Convertir el hash a una cadena base64.
		hashString := base64.StdEncoding.EncodeToString(hash[:])

		// Formatear el mensaje para enviarlo a los clientes.
		formattedMessage := fmt.Sprintf("[%s] %s: %s (%s)", message.time.Format("2006-01-02 15:04:05"), message.sender, message.body, hashString)

		// Enviar el mensaje a todos los clientes.
		for _, client := range clients {
			client.conn.Write([]byte(formattedMessage + "\n"))
		}
	}
}

// Función para iniciar el servidor de chat.
func StartChatServer() {
	// Iniciar una goroutine para manejar los mensajes de chat.
	go handleMessages()

	// Iniciar el servidor TCP.
	ln, err := net.Listen("tcp", fmt.Sprintf("%s:%d", *addr, *port))
	if err != nil {
		log.Fatal(err)
	}
	defer ln.Close()

	// Ciclo infinito para aceptar conexiones de clientes.
	for {
		// Aceptar una conexión de cliente.
		conn, err := ln.Accept()
		if err != nil {
			log.Println(err)
			continue
		}

		// Crear un nuevo cliente.
		client := &Client{conn: conn}

		// Leer el nombre del cliente.
		client.name, err = bufio.NewReader(conn).ReadString('\n')
		if err != nil {
			log.Println(err)
			conn.Close()
			continue
		}

		// Eliminar los espacios en blanco del nombre del cliente.
		client.name = strings.TrimSpace(client.name)

		// Si el nombre del cliente está vacío, cerrarlo.
		if client.name == "" {
			conn.Close()
			continue
		}

		// Agregar el cliente al mapa de clientes.
		mutex.Lock()
		clients[client.name] = client
		mutex.Unlock()

		// Enviar un mensaje de bienvenida al cliente.
		conn.Write([]byte("Bienvenido al chat.\n"))

		// Iniciar una goroutine para manejar los mensajes del cliente.
		go handleClient(client)
	}
}
```

**Explicación del código:**

* Se definen las variables de la línea de comandos, el puerto y la dirección del servidor.
* Se crea un servidor TCP y se inicia una goroutine para aceptar conexiones de clientes.
* Cada vez que un cliente se conecta, se crea un nuevo objeto `Client` y se agrega al mapa de clientes.
* Se inicia una goroutine para manejar los mensajes del cliente.
* Cada vez que un cliente envía un mensaje, se agrega al canal de mensajes.
* Una goroutine lee los mensajes del canal de mensajes y los envía a todos los clientes.

Este código es más complejo que el código de ejemplo anterior porque tiene las siguientes características:

* Utiliza un mapa para almacenar los clientes conectados.
* Utiliza un canal para almacenar los mensajes de chat.
* Utiliza una goroutine para manejar los mensajes de chat.
* Utiliza una función para iniciar el servidor de chat.