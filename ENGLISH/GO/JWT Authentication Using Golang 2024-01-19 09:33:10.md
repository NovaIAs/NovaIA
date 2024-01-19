package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"syscall"

	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
)

// Token is a JWT Token
type Token struct {
	UserID    int64
	Username  string
	IsAdmin   bool
	IssuedAt  int64
	ExpiresAt int64
}

// Claim is a representation of the JWT claim
type Claim struct {
	UserID    int64
	Username  string
	IsAdmin   bool
	IssuedAt  int64
	ExpiresAt int64
	jwt.StandardClaims
}

// Key is the secret key used to sign the JWT token
var Key = []byte("YOUR_SECRET_KEY")

// createToken creates a new JWT token
func createToken(userID int64, username string, isAdmin bool) (string, error) {
	claim := &Claim{
		UserID:    userID,
		Username:  username,
		IsAdmin:   isAdmin,
		IssuedAt:  time.Now().Unix(),
		ExpiresAt: time.Now().Add(time.Hour * 24).Unix(),
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claim)

	return token.SignedString(Key)
}

// validateToken validates a JWT token
func validateToken(tokenString string) (*Claim, error) {
	token, err := jwt.ParseWithClaims(tokenString, &Claim{}, func(token *jwt.Token) (interface{}, error) {
		return Key, nil
	})

	if err != nil {
		return nil, err
	}

	claim, ok := token.Claims.(*Claim)
	if !ok {
		return nil, fmt.Errorf("couldn't parse token claims")
	}

	return claim, nil
}

// protectedHandler is a middleware that protects a route
func protectedHandler(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Get the token from the request header
		tokenString := r.Header.Get("Authorization")

		// Validate the token
		claim, err := validateToken(tokenString)
		if err != nil {
			http.Error(w, http.StatusText(http.StatusUnauthorized), http.StatusUnauthorized)
			return
		}

		// Set the user ID in the request context
		ctx := context.WithValue(r.Context(), "userID", claim.UserID)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// main function
func main() {
	// Parse command-line arguments
	port := flag.Int("port", 8080, "Port to listen on")
	flag.Parse()

	// Create a new router
	router := mux.NewRouter()

	// Create a route for generating JWT tokens
	router.HandleFunc("/token", func(w http.ResponseWriter, r *http.Request) {
		// Parse the request body
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
			return
		}

		// Decode the JSON body
		var user User
		if err := json.Unmarshal(body, &user); err != nil {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
			return
		}

		// Create a new JWT token
		token, err := createToken(user.UserID, user.Username, user.IsAdmin)
		if err != nil {
			http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
			return
		}

		// Write the token to the response
		w.Write([]byte(token))
	})

	// Create a route for protected resources
	router.HandleFunc("/protected", protectedHandler(func(w http.ResponseWriter, r *http.Request) {
		// Get the user ID from the request context
		userID := r.Context().Value("userID").(int64)

		// Do something with the user ID
		fmt.Fprintf(w, "Hello, user %d!", userID)
	}))

	// Create a server
	server := &http.Server{
		Addr:    fmt.Sprintf(":%d", *port),
		Handler: router,
	}

	// Start the server
	go func() {
		log.Fatal(server.ListenAndServe())
	}()

	// Gracefully shutdown the server when a SIGINT or SIGTERM signal is received
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGINT, syscall.SIGTERM)
	<-c

	// Shut down the server
	server.Shutdown(context.Background())

	// Wait for the server to completely shut down
	err := server.Wait()
	if err != nil {
		log.Fatal(err)
	}
}

// User represents a user
type User struct {
	UserID    int64
	Username  string
	IsAdmin   bool
	Password  string
	FirstName string
	LastName  string
}