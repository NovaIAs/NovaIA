```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Define a structure to represent a task.
type Task struct {
	ID int
	Data string
}

// Define a channel to communicate between goroutines.
var tasks = make(chan Task)

// Define a wait group to wait for all goroutines to finish.
var wg sync.WaitGroup

// Define a function to generate random tasks.
func generateTasks(n int) {
	for i := 0; i < n; i++ {
		task := Task{
			ID:   i,
			Data: fmt.Sprintf("Task %d", i),
		}
		tasks <- task
	}
	close(tasks)
}

// Define a function to process tasks.
func processTasks(workerID int) {
	for task := range tasks {
		// Simulate processing time.
		time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
		fmt.Printf("Worker %d processed task %d (%s)\n", workerID, task.ID, task.Data)
		wg.Done()
	}
}

// Define a function to create and start workers.
func createWorkers(n int) {
	for i := 0; i < n; i++ {
		wg.Add(1)
		go processTasks(i)
	}
}

func main() {
	// Generate 100 random tasks.
	generateTasks(100)

	// Create 5 workers to process the tasks.
	createWorkers(5)

	// Wait for all workers to finish.
	wg.Wait()

	fmt.Println("All tasks processed.")
}
```

Explanation:

1. **Data Structure**: We define a `Task` struct to represent a task with an ID and data.

2. **Channel**: We create a channel named `tasks` to communicate between goroutines. Channels are used for concurrent communication in Go.

3. **Task Generation**: The `generateTasks` function generates a specified number of random tasks and sends them to the `tasks` channel.

4. **Task Processing**: The `processTasks` function is a goroutine that continuously receives tasks from the `tasks` channel, simulates processing time, and prints the processed task information.

5. **Worker Creation**: The `createWorkers` function creates a specified number of worker goroutines that call the `processTasks` function.

6. **Main Function**: In the `main` function, we call the `generateTasks` and `createWorkers` functions to generate and process tasks concurrently. We use a wait group to ensure that all goroutines finish before the program exits.

7. **Output**: The program prints the processed task information, including the worker ID, task ID, and task data.

This code demonstrates concurrent task processing using goroutines and channels in Go. It simulates a scenario where multiple workers are processing a large number of tasks in parallel.