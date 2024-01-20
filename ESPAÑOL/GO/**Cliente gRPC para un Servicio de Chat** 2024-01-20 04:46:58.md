```go
// Importar los paquetes necesarios.
import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/golang/protobuf/ptypes/timestamp"
	"google.golang.org/grpc"

	pb "grpc-ejemplo-golang/proto"
)

// Definir el cliente gRPC.
var client pb.GreeterClient

// Conectar al servidor gRPC.
func connect() {
	// Obtener el puerto del servidor del entorno.
	port := os.Getenv("PORT")

	// Crear una conexión con el servidor.
	conn, err := grpc.Dial(fmt.Sprintf("localhost:%s", port), grpc.WithInsecure())
	if err != nil {
		log.Fatalf("No se pudo conectar al servidor: %v", err)
	}

	// Crear un cliente gRPC.
	client = pb.NewGreeterClient(conn)
}

// Saludo al servidor.
func saludar(nombre string) {
	// Crear una solicitud de saludo.
	req := &pb.HelloRequest{
		Name: nombre,
	}

	// Enviar la solicitud al servidor.
	resp, err := client.SayHello(context.Background(), req)
	if err != nil {
		log.Fatalf("No se pudo saludar al servidor: %v", err)
	}

	// Imprimir la respuesta del servidor.
	fmt.Printf("El servidor respondió: %s\n", resp.Message)
}

// Despedida al servidor.
func despedida(nombre string) {
	// Crear una solicitud de despedida.
	req := &pb.GoodbyeRequest{
		Name: nombre,
	}

	// Enviar la solicitud al servidor.
	resp, err := client.SayGoodbye(context.Background(), req)
	if err != nil {
		log.Fatalf("No se pudo despedir del servidor: %v", err)
	}

	// Imprimir la respuesta del servidor.
	fmt.Printf("El servidor respondió: %s\n", resp.Message)
}

// Obtener la hora del servidor.
func obtenerHora() {
	// Crear una solicitud para obtener la hora.
	req := &pb.TimeRequest{}

	// Enviar la solicitud al servidor.
	resp, err := client.GetTime(context.Background(), req)
	if err != nil {
		log.Fatalf("No se pudo obtener la hora del servidor: %v", err)
	}

	// Imprimir la respuesta del servidor.
	fmt.Printf("La hora actual es: %s\n", resp.Time.AsTime())
}

// Crear un nuevo usuario.
func crearUsuario(nombre, email string) {
	// Crear una solicitud para crear un nuevo usuario.
	req := &pb.CreateUserRequest{
		User: &pb.User{
			Name:  nombre,
			Email: email,
		},
	}

	// Enviar la solicitud al servidor.
	resp, err := client.CreateUser(context.Background(), req)
	if err != nil {
		log.Fatalf("No se pudo crear un nuevo usuario: %v", err)
	}

	// Imprimir la respuesta del servidor.
	fmt.Printf("El nuevo usuario se creó correctamente: %s\n", resp.User.Name)
}

// Obtener los usuarios.
func obtenerUsuarios() {
	// Crear una solicitud para obtener los usuarios.
	req := &pb.GetUsersRequest{}

	// Enviar la solicitud al servidor.
	stream, err := client.GetUsers(context.Background(), req)
	if err != nil {
		log.Fatalf("No se pudieron obtener los usuarios: %v", err)
	}

	// Recibir los usuarios del servidor.
	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("No se pudo recibir los usuarios: %v", err)
		}

		// Imprimir el nombre del usuario.
		fmt.Printf("Usuario: %s\n", resp.User.Name)
	}
}

// Main.
func main() {
	// Conectar al servidor gRPC.
	connect()

	// Saludo al servidor.
	saludar("John")

	// Despedida al servidor.
	despedida("John")

	// Obtener la hora del servidor.
	obtenerHora()

	// Crear un nuevo usuario.
	crearUsuario("Jane", "jane@example.com")

	// Obtener los usuarios.
	obtenerUsuarios()
}
```

Explicación del código:

* Se importan los paquetes necesarios.
* Se define el cliente gRPC.
* Se conecta al servidor gRPC.
* Se definen las funciones para saludar, despedirse, obtener la hora, crear un nuevo usuario y obtener los usuarios.
* Se llama a las funciones para saludar, despedirse, obtener la hora, crear un nuevo usuario y obtener los usuarios.

El código es bastante complejo y abarca muchos aspectos de la programación en Go, como la gestión de errores, la concurrencia y la comunicación con servicios remotos.