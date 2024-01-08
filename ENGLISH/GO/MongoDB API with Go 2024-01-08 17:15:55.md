```go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"
    "os"
    "os/signal"
    "time"

    "github.com/gorilla/mux"
    "go.mongodb.org/mongo-driver/mongo"
    "go.mongodb.org/mongo-driver/mongo/options"
)

var client *mongo.Client

func main() {
    // Establish a connection to the MongoDB server
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    connectionString := os.Getenv("MONGODB_URI")
    if connectionString == "" {
        log.Fatal("MONGODB_URI environment variable is not set")
    }

    clientOptions := options.Client().ApplyURI(connectionString)
    var err error
    client, err = mongo.Connect(ctx, clientOptions)
    if err != nil {
        log.Fatal(err)
    }

    // Initialize the HTTP server
    router := mux.NewRouter()
    router.HandleFunc("/", homePage).Methods("GET")
    router.HandleFunc("/products", getProducts).Methods("GET")
    router.HandleFunc("/products/{id}", getProductByID).Methods("GET")
    router.HandleFunc("/products", createProduct).Methods("POST")
    router.HandleFunc("/products/{id}", updateProduct).Methods("PUT")
    router.HandleFunc("/products/{id}", deleteProduct).Methods("DELETE")

    // Create a server and start listening for requests
    srv := &http.Server{
        Handler: router,
        Addr:    ":8080",
    }

    // Start the server in a goroutine so that it doesn't block the main thread
    go func() {
        if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            log.Fatal(err)
        }
    }()

    // Handle graceful shutdown of the server
    quit := make(chan os.Signal)
    signal.Notify(quit, os.Interrupt)
    <-quit

    // Shut down the server gracefully
    ctx, cancel = context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()
    if err := srv.Shutdown(ctx); err != nil {
        log.Fatal(err)
    }

    log.Println("Server shutdown successfully")
}

// homePage displays a welcome message
func homePage(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintf(w, "Welcome to the API!")
}

// getProducts gets all products from the database
func getProducts(w http.ResponseWriter, r *http.Request) {
    collection := client.Database("products").Collection("products")
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    cursor, err := collection.Find(ctx, nil)
    if err != nil {
        http.Error(w, "Error getting products", http.StatusInternalServerError)
        return
    }

    var products []Product
    if err := cursor.All(ctx, &products); err != nil {
        http.Error(w, "Error getting products", http.StatusInternalServerError)
        return
    }

    jsonResponse, err := json.Marshal(products)
    if err != nil {
        http.Error(w, "Error converting products to JSON", http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json")
    w.Write(jsonResponse)
}

// getProductByID gets a product by its ID
func getProductByID(w http.ResponseWriter, r *http.Request) {
    params := mux.Vars(r)
    id := params["id"]

    collection := client.Database("products").Collection("products")
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    var product Product
    if err := collection.FindOne(ctx, bson.M{"_id": id}).Decode(&product); err != nil {
        http.Error(w, "Error getting product", http.StatusInternalServerError)
        return
    }

    jsonResponse, err := json.Marshal(product)
    if err != nil {
        http.Error(w, "Error converting product to JSON", http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json")
    w.Write(jsonResponse)
}

// createProduct creates a new product
func createProduct(w http.ResponseWriter, r *http.Request) {
    var product Product
    decoder := json.NewDecoder(r.Body)
    if err := decoder.Decode(&product); err != nil {
        http.Error(w, "Error decoding product", http.StatusBadRequest)
        return
    }

    collection := client.Database("products").Collection("products")
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    result, err := collection.InsertOne(ctx, product)
    if err != nil {
        http.Error(w, "Error creating product", http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json")
    jsonResponse, err := json.Marshal(result.InsertedID)
    if err != nil {
        http.Error(w, "Error converting ID to JSON", http.StatusInternalServerError)
        return