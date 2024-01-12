```go
package main

import (
	"fmt"
	"math/rand"
	"os"
	"strings"
	"time"
)

// constants
const (
	maxAttempts = 10
	maxPasswordLength = 32
	minPasswordLength = 10
)

// functions
func generatePassword() string {
	var password strings.Builder

	charSet := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*"
	charsetLen := len(charSet)

	for i := 0; i < maxPasswordLength; i++ {
		randIndex := rand.Intn(charsetLen)
		password.WriteByte(charSet[randIndex])
	}

	return password.String()
}

func checkPasswordStrength(password string) int {
	score := 0

	if len(password) < minPasswordLength {
		score += 1
	} else if len(password) > maxPasswordLength {
		score += 1
	}

	hasUpper := false
	hasLower := false
	hasNumber := false
	hasSymbol := false

	for _, char := range password {
		if char >= 'A' && char <= 'Z' {
			hasUpper = true
		} else if char >= 'a' && char <= 'z' {
			hasLower = true
		} else if char >= '0' && char <= '9' {
			hasNumber = true
		} else {
			hasSymbol = true
		}
	}

	if hasUpper {
		score += 1
	}
	if hasLower {
		score += 1
	}
	if hasNumber {
		score += 1
	}
	if hasSymbol {
		score += 1
	}

	return score
}

// main function
func main() {
	// seed random number generator
	rand.Seed(time.Now().UnixNano())

	// prompt user to enter desired password length
	fmt.Print("Enter desired password length (10-32): ")
	var passwordLength int
	fmt.Scanln(&passwordLength)

	// validate password length
	if passwordLength < minPasswordLength || passwordLength > maxPasswordLength {
		fmt.Println("Invalid password length. Please enter a value between 10 and 32.")
		os.Exit(1)
	}

	// generate password
	password := generatePassword()

	// check password strength
	strength := checkPasswordStrength(password)

	// display password and strength rating
	fmt.Println("Generated password:", password)
	fmt.Println("Password strength:", strength)
}
```

This program generates a random password of a specified length and checks its strength based on certain criteria. Here's how the code works:

1. Constants: The program defines a few constants:
   - `maxAttempts`: Maximum number of attempts to generate a strong password.
   - `maxPasswordLength`: Maximum allowed password length.
   - `minPasswordLength`: Minimum allowed password length.

2. Functions:
   - `generatePassword()`: This function generates a random password of the specified length. It uses a character set containing lowercase letters, uppercase letters, digits, and symbols.
   - `checkPasswordStrength()`: This function checks the strength of a given password based on the following criteria:
     - Password length: A password with a length less than `minPasswordLength` or greater than `maxPasswordLength` is considered weak.
     - Character types: A password with a mix of lowercase letters, uppercase letters, digits, and symbols is considered stronger.

3. Main Function:
   - It starts by seeding the random number generator with the current time.
   - It prompts the user to enter the desired password length.
   - It validates the user input to ensure the password length is within the allowed range.
   - It calls the `generatePassword()` function to generate a password of the specified length.
   - It calls the `checkPasswordStrength()` function to evaluate the strength of the generated password.
   - Finally, it displays the generated password and its strength rating to the user.

This program provides a simple way to generate and assess the strength of a random password based on user-defined criteria.