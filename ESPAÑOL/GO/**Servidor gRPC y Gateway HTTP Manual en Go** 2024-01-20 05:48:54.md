```go
package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/golang/protobuf/ptypes/wrappers"

	example "github.com/grpc-ecosystem/grpc-gateway/examples/manual"
	examplepb "github.com/grpc-ecosystem/grpc-gateway/examples/manual/examplepb"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"
)

// Creamos una variable global que almacena el servicio gRPC.
var svc example.SimpleServiceServer = example.NewSimpleService()

// Creamos una variable global que almacena el servidor HTTP.
var httpGatewayServer *http.Server

// La función principal que se ejecuta cuando se inicia el programa.
func main() {
	// Creamos un contexto para manejar la cancelación de las goroutines.
	ctx := context.Background()

	// Creamos un nuevo servidor gRPC.
	grpcServer := grpc.NewServer()

	// Registramos el servicio gRPC en el servidor gRPC.
	examplepb.RegisterSimpleServiceServer(grpcServer, svc)

	// Creamos un servidor HTTP que utilice el gateway gRPC.
	httpGatewayServer = &http.Server{
		Addr: ":8080",
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Accedemos al contexto de la petición HTTP.
			ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second)
			defer cancel()

			// Creamos una nueva instancia del cliente gRPC.
			conn, err := grpc.DialContext(ctx, "localhost:8081", grpc.WithInsecure())
			if err != nil {
				http.Error(w, "Error al conectar con el servicio gRPC", http.StatusInternalServerError)
				return
			}
			defer conn.Close()

			// Creamos un nuevo cliente para el servicio gRPC.
			client := examplepb.NewSimpleServiceClient(conn)

			// Hacemos una petición al servicio gRPC.
			resp, err := client.Echo(ctx, &examplepb.SimpleRequest{Message: "Hola desde el gateway HTTP"})
			if err != nil {
				http.Error(w, "Error al hacer la petición al servicio gRPC", http.StatusInternalServerError)
				return
			}

			// Devolvemos la respuesta al cliente HTTP.
			fmt.Fprintf(w, "Respuesta del servicio gRPC: %s", resp.Message)
		}),
	}

	// Creamos un canal para recibir las señales de interrupción.
	stop := make(chan os.Signal, 1)
	signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)

	// Iniciamos el servidor gRPC en una nueva goroutine.
	go func() {
		log.Printf("Iniciando el servidor gRPC en :8081")
		if err := grpcServer.Serve(net.Listen("tcp", ":8081")); err != nil {
			log.Fatalf("Error al iniciar el servidor gRPC: %v", err)
		}
	}()

	// Iniciamos el servidor HTTP en una nueva goroutine.
	go func() {
		log.Printf("Iniciando el servidor HTTP en :8080")
		if err := httpGatewayServer.ListenAndServe