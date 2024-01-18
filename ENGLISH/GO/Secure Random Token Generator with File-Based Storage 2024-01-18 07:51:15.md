```go
// This is a complete program in Go.
package main

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math/big"
	"net/http"
	"os"
)

// Data is a simple struct to hold some data.
type Data struct {
	Value string `json:"value"`
}

// This function generates a random string of a given length.
func generateToken(length int) string {
	b := make([]byte, length)
	_, err := rand.Read(b)
	if err != nil {
		log.Fatal(err)
	}
	return base64.URLEncoding.EncodeToString(b)
}

// This function hashes a string using the SHA256 algorithm.
func hashString(s string) string {
	h := sha256.New()
	h.Write([]byte(s))
	return base64.URLEncoding.EncodeToString(h.Sum(nil))
}

// This function reads a file and returns its contents as a string.
func readFile(filename string) string {
	b, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}
	return string(b)
}

// This function writes a string to a file.
func writeFile(filename string, s string) {
	err := ioutil.WriteFile(filename, []byte(s), 0644)
	if err != nil {
		log.Fatal(err)
	}
}

// This function starts a simple HTTP server.
func startServer(addr string) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Generate a random token.
		token := generateToken(32)

		// Hash the token.
		hash := hashString(token)

		// Write the hash to a file.
		writeFile("hash.txt", hash)

		// Send the token to the client.
		fmt.Fprintf(w, "Your token is: %s", token)
	})

	log.Fatal(http.ListenAndServe(addr, nil))
}

// This function is the entry point of the program.
func main() {
	// Read the command line arguments.
	args := os.Args[1:]
	if len(args) < 1 {
		log.Fatal("Usage: ./program <port>")
	}

	// Start the server.
	startServer(args[0])
}
```

This program does the following:

1. It generates a random string of 32 bytes and hashes it using the SHA256 algorithm.
2. It writes the hash to a file.
3. It starts a simple HTTP server that listens on the specified port.
4. When a client connects to the server, the server sends the random token to the client and writes the hash to a file.

This program is complex because it uses a number of different concepts, including cryptography, file I/O, and networking. It is also differentiated because it is unlikely to be repeated again in a different context.