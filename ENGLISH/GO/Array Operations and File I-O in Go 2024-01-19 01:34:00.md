```go
// Main function
package main

import (
	"fmt"
	"math/rand"
	"os"
	"time"
)

// Define constants
const (
	MAX_ITERATIONS   = 10
	MAX_ARRAY_SIZE   = 100
	MAX_RANDOM_VALUE = 1000
)

// Define a custom error type
type MyError struct {
	Message string
}

func (e *MyError) Error() string {
	return e.Message
}

// This function takes an array of integers and returns the sum of all the elements in the array
func sumArray(array []int) int {
	sum := 0
	for _, value := range array {
		sum += value
	}
	return sum
}

// This function takes an array of integers and returns the average of all the elements in the array
func avgArray(array []int) float64 {
	sum := sumArray(array)
	return float64(sum) / float64(len(array))
}

// This function takes an array of integers and returns the maximum value in the array
func maxArray(array []int) int {
	max := array[0]
	for _, value := range array {
		if value > max {
			max = value
		}
	}
	return max
}

// This function takes an array of integers and returns the minimum value in the array
func minArray(array []int) int {
	min := array[0]
	for _, value := range array {
		if value < min {
			min = value
		}
	}
	return min
}

// This function takes an array of integers and returns a new array containing only the unique values from the original array
func uniqueArray(array []int) []int {
	uniqueValues := make([]int, 0)
	for _, value := range array {
		if !contains(uniqueValues, value) {
			uniqueValues = append(uniqueValues, value)
		}
	}
	return uniqueValues
}

// This function checks if a value is contained in an array
func contains(array []int, value int) bool {
	for _, v := range array {
		if v == value {
			return true
		}
	}
	return false
}

// This function generates a random array of integers with a given size and maximum value
func generateRandomArray(size, maxValue int) []int {
	rand.Seed(time.Now().UnixNano())
	array := make([]int, size)
	for i := 0; i < size; i++ {
		array[i] = rand.Intn(maxValue)
	}
	return array
}

// Main function
func main() {
	// Create a random array of integers
	array := generateRandomArray(MAX_ARRAY_SIZE, MAX_RANDOM_VALUE)

	// Print the original array
	fmt.Println("Original array:", array)

	// Calculate the sum of the array
	sum := sumArray(array)
	fmt.Println("Sum of the array:", sum)

	// Calculate the average of the array
	avg := avgArray(array)
	fmt.Println("Average of the array:", avg)

	// Find the maximum value in the array
	max := maxArray(array)
	fmt.Println("Maximum value in the array:", max)

	// Find the minimum value in the array
	min := minArray(array)
	fmt.Println("Minimum value in the array:", min)

	// Find the unique values in the array
	uniqueValues := uniqueArray(array)
	fmt.Println("Unique values in the array:", uniqueValues)

	// Write the unique values to a file
	err := writeToFile("unique_values.txt", uniqueValues)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	fmt.Println("Unique values written to file successfully")
}

// This function writes a slice of integers to a file
func writeToFile(filename string, data []int) error {
	file, err := os.Create(filename)
	if err != nil {
		return &MyError{Message: "Could not create file"}
	}
	defer file.Close()

	for _, value := range data {
		_, err := file.WriteString(fmt.Sprintf("%d\n", value))
		if err != nil {
			return &MyError{Message: "Could not write to file"}
		}
	}

	return nil
}
```

Explanation:

1. The code starts with the `package main` statement, which indicates that this is the main program file.
2. It then imports the necessary libraries:
    * `fmt`: for input and output operations
    * `math/rand`: for generating random numbers
    * `os`: for file operations
    * `time`: for generating a random seed
3. Various constants are defined, such as the maximum number of iterations, the maximum size of arrays, and the maximum value of random numbers.
4. A custom error type, `MyError`, is defined to handle errors in the program.
5. Several helper functions are defined:
    * `sumArray`: calculates the sum of an array of integers
    * `avgArray`: calculates the average of an array of integers
    * `maxArray`: finds the maximum value in an array of integers
    * `minArray`: finds the minimum value in an array of integers
    * `uniqueArray`: finds the unique values in an array of integers
    * `contains`: checks if a value is contained in an array of integers
    * `generateRandomArray`: generates a random array of integers with a given size and maximum value
    * `writeToFile`: writes a slice of integers to a file
6. The main function is defined, which is the entry point of the program.
7. Inside the main function, a random array of integers is generated and printed to the console.
8. Various calculations are performed on the array, such as calculating the sum, average, maximum, minimum, and unique values.
9. The unique values are then written to a file named `unique_values.txt`.
10. The program terminates with a success or error message.

This code showcases various programming techniques, including:

* Defining constants and custom error types
* Writing helper functions to modularize the code
* Generating and manipulating arrays of integers
* Performing various calculations on arrays
* Writing data to a file