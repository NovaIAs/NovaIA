```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Constants
const (
	// Number of goroutines to use
	numGoroutines = 10

	// Total number of elements to process
	totalElements = 100000

	// Number of elements to process per goroutine
	elementsPerGoroutine = totalElements / numGoroutines
)

// Data structure to hold the results
type Result struct {
	sum int
	err error
}

// Function to process a slice of elements
func processElements(elements []int, results chan Result) {
	sum := 0
	for _, element := range elements {
		sum += element
	}
	results <- Result{sum, nil}
}

func main() {
	start := time.Now()

	// Initialize the results channel
	results := make(chan Result, numGoroutines)

	// Create a wait group to wait for all goroutines to finish
	var wg sync.WaitGroup
	wg.Add(numGoroutines)

	// Generate a slice of random integers
	elements := make([]int, totalElements)
	for i := range elements {
		elements[i] = rand.Intn(100)
	}

	// Split the slice of elements into chunks and process each chunk in a separate goroutine
	for i := 0; i < numGoroutines; i++ {
		go func(i int) {
			defer wg.Done()
			start := i * elementsPerGoroutine
			end := start + elementsPerGoroutine
			processElements(elements[start:end], results)
		}(i)
	}

	// Wait for all goroutines to finish
	wg.Wait()

	// Close the results channel
	close(results)

	// Collect the results from the channel
	totalSum := 0
	for result := range results {
		if result.err != nil {
			fmt.Println("Error:", result.err)
			continue
		}
		totalSum += result.sum
	}

	// Print the total sum
	fmt.Println("Total sum:", totalSum)

	elapsed := time.Since(start)
	fmt.Println("Elapsed time:", elapsed)
}
```

Explanation:

1. Import necessary libraries.

2. Define constants for the number of goroutines, total elements to process, and elements per goroutine.

3. Define a `Result` struct to hold the sum and error for each goroutine.

4. Define a `processElements` function that takes a slice of elements and a channel to send the result to. This function calculates the sum of the elements and sends the result to the channel.

5. In the `main` function, create a channel to receive the results from the goroutines.

6. Create a wait group to wait for all goroutines to finish.

7. Generate a slice of random integers.

8. Split the slice of elements into chunks and process each chunk in a separate goroutine.

9. Wait for all goroutines to finish using the wait group.

10. Close the results channel to indicate that no more results will be sent.

11. Collect the results from the channel and calculate the total sum.

12. Print the total sum and the elapsed time.