```go
package main

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"log"
	"math/rand"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Predefined data
const (
	minPasswordLength = 10
	maxPasswordLength = 20
	charset           = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*"
)

// Function to generate a random password
func generatePassword() string {
	// Initialize the random number generator
	rand.Seed(time.Now().UnixNano())

	// Generate a random password of length between minPasswordLength and maxPasswordLength
	passwordLength := minPasswordLength + rand.Intn(maxPasswordLength-minPasswordLength+1)
	password := make([]byte, passwordLength)

	// Generate random bytes and select characters from the charset
	for i := range password {
		password[i] = charset[rand.Intn(len(charset))]
	}

	// Return the generated password as a string
	return string(password)
}

// Function to check if a password is strong
func isStrongPassword(password string) bool {
	// Check if the password meets the minimum length requirement
	if len(password) < minPasswordLength {
		return false
	}

	// Check if the password contains at least one uppercase letter
	uppercaseRegex := regexp.MustCompile("[A-Z]")
	if !uppercaseRegex.MatchString(password) {
		return false
	}

	// Check if the password contains at least one lowercase letter
	lowercaseRegex := regexp.MustCompile("[a-z]")
	if !lowercaseRegex.MatchString(password) {
		return false
	}

	// Check if the password contains at least one digit
	digitRegex := regexp.MustCompile("[0-9]")
	if !digitRegex.MatchString(password) {
		return false
	}

	// Check if the password contains at least one special character
	specialCharRegex := regexp.MustCompile("[!@#$%^&*]")
	if !specialCharRegex.MatchString(password) {
		return false
	}

	// The password meets all the requirements, so it is strong
	return true
}

// Function to generate a SHA256 hash of a password
func generatePasswordHash(password string) string {
	// Create a new SHA256 hasher
	hasher := sha256.New()

	// Write the password to the hasher
	if _, err := hasher.Write([]byte(password)); err != nil {
		log.Fatalf("Error writing password to hasher: %v", err)
	}

	// Get the hash as a byte slice
	hash := hasher.Sum(nil)

	// Convert the hash to a hex string
	hashString := hex.EncodeToString(hash)

	// Return the hash string
	return hashString
}

// Function to verify a password against a hash
func verifyPassword(password, hash string) bool {
	// Generate a new SHA256 hasher
	hasher := sha256.New()

	// Write the password to the hasher
	if _, err := hasher.Write([]byte(password)); err != nil {
		log.Fatalf("Error writing password to hasher: %v", err)
	}

	// Get the hash as a byte slice
	hashBytes := hasher.Sum(nil)

	// Convert the hash bytes to a hex string
	hashString := hex.EncodeToString(hashBytes)

	// Compare the generated hash with the provided hash
	return hashString == hash
}

// Function to get a password from the user
func getPassword() string {
	// Create a new buffered reader to read from the standard input
	reader := bufio.NewReader(os.Stdin)

	// Prompt the user to enter a password
	fmt.Print("Enter a password: ")

	// Read the password from the standard input
	password, err := reader.ReadString('\n')
	if err != nil {
		log.Fatalf("Error reading password: %v", err)
	}

	// Trim any whitespace from the password
	password = strings.TrimSpace(password)

	// Return the password
	return password
}

// Function to save a password hash to a file
func savePasswordHash(hash string) {
	// Open a file for writing
	file, err := os.OpenFile("password.hash", os.O_CREATE|os.O_WRONLY, 0600)
	if err != nil {
		log.Fatalf("Error opening file: %v", err)
	}

	// Defer closing the file
	defer file.Close()

	// Write the hash to the file
	if _, err := file.WriteString(hash); err != nil {
		log.Fatalf("Error writing hash to file: %v", err)
	}
}

// Function to load a password hash from a file
func loadPasswordHash() string {
	// Open a file for reading
	file, err := os.Open("password.hash")
	if err != nil {
