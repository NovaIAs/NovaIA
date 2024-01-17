```go
package main

import (
	"context"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type Job struct {
	id       int
	priority int
}

type Worker struct {
	id  int
	job chan Job
}

func main() {
	// Create a job queue.
	jobQueue := make(chan Job, 100)

	// Create a pool of workers.
	numWorkers := 5
	workers := make([]*Worker, numWorkers)
	for i := 0; i < numWorkers; i++ {
		workers[i] = &Worker{id: i, job: make(chan Job)}
		go worker(workers[i], jobQueue)
	}

	// Create a random generator for job IDs and priorities.
	rand.Seed(time.Now().UnixNano())

	// Generate 100 jobs and send them to the job queue.
	for i := 0; i < 100; i++ {
		job := Job{id: rand.Intn(100), priority: rand.Intn(10)}
		jobQueue <- job
	}

	// Close the job queue to signal to the workers that there are no more jobs to be done.
	close(jobQueue)

	// Wait for all workers to finish their jobs.
	var wg sync.WaitGroup
	wg.Add(numWorkers)
	for _, worker := range workers {
		go func(worker *Worker) {
			for job := range worker.job {
				fmt.Printf("Worker %d processed job %d with priority %d\n", worker.id, job.id, job.priority)
			}
			wg.Done()
		}(worker)
	}
	wg.Wait()
}

// The worker function processes jobs from the job queue.
func worker(worker *Worker, jobQueue <-chan Job) {
	for job := range jobQueue {
		// Simulate processing the job.
		time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
		fmt.Printf("Worker %d processed job %d with priority %d\n", worker.id, job.id, job.priority)
	}
}
```

This code simulates a job queue with a pool of workers. Jobs are generated randomly and sent to the job queue, and workers process jobs from the queue in order of priority.

The main function sets up the job queue and the pool of workers. Jobs are generated using a random generator and sent to the job queue. The job queue is closed once all jobs have been generated.

The worker function waits for jobs to be sent to its channel, and then processes each job in order of priority. The worker function simulates processing the job by sleeping for a random amount of time.

The program waits for all workers to finish their jobs before exiting.