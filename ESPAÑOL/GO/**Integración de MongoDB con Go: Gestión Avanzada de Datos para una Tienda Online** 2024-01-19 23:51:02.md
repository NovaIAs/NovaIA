```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/joho/godotenv"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

// Modelo de datos de usuario.
type Usuario struct {
	ID primitive.ObjectID `bson:"_id,omitempty"`
	Nombre string `bson:"nombre,omitempty"`
	Edad int `bson:"edad,omitempty"`
	Correo string `bson:"correo,omitempty"`
}

// Modelo de datos de producto.
type Producto struct {
	ID primitive.ObjectID `bson:"_id,omitempty"`
	Nombre string `bson:"nombre,omitempty"`
	Precio float32 `bson:"precio,omitempty"`
	Cantidad int `bson:"cantidad,omitempty"`
}

// Modelo de datos de pedido.
type Pedido struct {
	ID primitive.ObjectID `bson:"_id,omitempty"`
	UsuarioID primitive.ObjectID `bson:"usuario_id,omitempty"`
	Productos []Producto `bson:"productos,omitempty"`
	Total float32 `bson:"total,omitempty"`
	Fecha time.Time `bson:"fecha,omitempty"`
}

// Función para conectar a la base de datos MongoDB.
func connectMongoDB() (*mongo.Client, error) {
	err := godotenv.Load(".env")
	if err != nil {
		log.Fatal("Error al cargar el archivo .env")
	}

	uri := os.Getenv("MONGODB_URI")
	clientOptions := options.Client().ApplyURI(uri)
	client, err := mongo.Connect(context.TODO(), clientOptions)
	if err != nil {
		return nil, fmt.Errorf("Error al conectar a la base de datos: %v", err)
	}

	err = client.Ping(context.TODO(), nil)
	if err != nil {
		return nil, fmt.Errorf("Error al hacer ping a la base de datos: %v", err)
	}

	return client, nil
}

// Función para crear un nuevo usuario.
func crearUsuario(client *mongo.Client, nombre, correo string, edad int) (*Usuario, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	usuario := &Usuario{
		Nombre: nombre,
		Edad: edad,
		Correo: correo,
	}

	collection := client.Database("tienda").Collection("usuarios")
	result, err := collection.InsertOne(ctx, usuario)
	if err != nil {
		return nil, fmt.Errorf("Error al crear el usuario: %v", err)
	}

	usuario.ID = result.InsertedID.(primitive.ObjectID)
	return usuario, nil
}

// Función para obtener todos los usuarios.
func obtenerUsuarios(client *mongo.Client) ([]*Usuario, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	collection := client.Database("tienda").Collection("usuarios")
	cursor, err := collection.Find(ctx, bson.M{})
	if err != nil {
		return nil, fmt.Errorf("Error al obtener los usuarios: %v", err)
	}

	var usuarios []*Usuario
	for cursor.Next(ctx) {
		usuario := &Usuario{}
		err := cursor.Decode(usuario)
		if err != nil {
			return nil, fmt.Errorf("Error al decodificar el usuario: %v", err)
		}

		usuarios = append(usuarios, usuario)
	}

	return usuarios, nil
}

// Función para crear un nuevo producto.
func crearProducto(client *mongo.Client, nombre string, precio float32, cantidad int) (*Producto, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	producto := &Producto{
		Nombre: nombre,
		Precio: precio,
		Cantidad: cantidad,
	}

	collection := client.Database("tienda").Collection("productos")
	result, err := collection.InsertOne(ctx, producto)
	if err != nil {
		return nil, fmt.Errorf("Error al crear el producto: %v", err)
	}

	producto.ID = result.InsertedID.(primitive.ObjectID)
	return producto, nil
}

// Función para obtener todos los productos.
func obtenerProductos(client *mongo.Client) ([]*Producto, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	collection := client.Database("tienda").Collection("productos")
	cursor, err := collection.Find(ctx, bson.M{})
	if err != nil {
		return nil, fmt.Errorf("Error al obtener los productos: %v", err)
	}

	var productos []*Producto
	for cursor.Next(ctx) {
		producto := &Producto{}
		err := cursor.Decode(producto)
		if err != nil {
			return nil, fmt.Errorf("Error al decodificar el producto: %v", err)
		}

		productos = append(productos, producto)
	}

	return productos, nil
}

// Función para crear un nuevo pedido.
func crearPedido(client *mongo.Client, usuarioID primitive.ObjectID, productos []Producto) (*Pedido, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	var total float32
	for _, producto := range productos {
		total += producto.Precio * float32(producto.Cantidad)
	}

	pedido := &Pedido{
		UsuarioID: usuarioID,
		Productos: productos,
		Total: total,
		Fecha: time.Now(),
	}

	collection := client.Database("tienda").Collection("pedidos")
	result, err := collection.InsertOne(ctx, pedido)
	if err != nil {
		return nil, fmt.Errorf("Error al crear el pedido: %v", err)
	}

	pedido.ID = result.InsertedID.(primitive.ObjectID)
	return pedido, nil
}

// Función para obtener todos los pedidos.
func obtenerPedidos(client *mongo.Client) ([]*Pedido, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	collection := client.Database("tienda").Collection("pedidos")
	cursor, err := collection.Find(ctx, bson.M{})
	if err != nil {
		return nil, fmt.Errorf("Error al obtener los pedidos: %v", err)
	}

	var pedidos []*Pedido
	for cursor.Next(ctx) {
		pedido := &Pedido{}
		err := cursor.Decode(pedido)
		if err != nil {
			return nil, fmt.Errorf("Error al decodificar el pedido: %v", err)
		}

		pedidos = append(pedidos, pedido)
	}

	return pedidos, nil
}

// Función para manejar la petición POST de creación de un nuevo usuario.
func crearUsuarioHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, http.StatusMethodNotAllowed, http.StatusText(http.StatusMethodNotAllowed))
		return
	}

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	var usuario Usuario
	err = json.Unmarshal(body, &usuario)
	if err != nil {
		http.Error(w, http.StatusBadRequest, http.StatusText(http.StatusBadRequest))
		return
	}

	client, err := connectMongoDB()
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	usuario, err = crearUsuario(client, usuario.Nombre, usuario.Correo, usuario.Edad)
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	json.NewEncoder(w).Encode(usuario)
}

// Función para manejar la petición GET de obtención de todos los usuarios.
func obtenerUsuariosHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, http.StatusMethodNotAllowed, http.StatusText(http.StatusMethodNotAllowed))
		return
	}

	client, err := connectMongoDB()
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	usuarios, err := obtenerUsuarios(client)
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(usuarios)
}

// Función para manejar la petición POST de creación de un nuevo producto.
func crearProductoHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, http.StatusMethodNotAllowed, http.StatusText(http.StatusMethodNotAllowed))
		return
	}

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, http.StatusInternalServerError, http.StatusText(http.StatusInternalServerError))
		return
	}

	var producto Producto
	err = json.Unmarshal(body, &producto