```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Philosopher struct represents a philosopher.
type Philosopher struct {
	name      string
	leftFork  *Chopstick
	rightFork *Chopstick
	mealsEaten int
}

// Chopstick struct represents a chopstick.
type Chopstick struct {
	sync.Mutex
}

// createPhilosopher creates a new philosopher.
func createPhilosopher(name string, leftFork, rightFork *Chopstick) *Philosopher {
	return &Philosopher{
		name:      name,
		leftFork:  leftFork,
		rightFork: rightFork,
	}
}

// eat simulates a philosopher eating.
func (p *Philosopher) eat() {
	fmt.Printf("%s is eating.\n", p.name)
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
	fmt.Printf("%s has finished eating.\n", p.name)
	p.mealsEaten++
}

// think simulates a philosopher thinking.
func (p *Philosopher) think() {
	fmt.Printf("%s is thinking.\n", p.name)
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
	fmt.Printf("%s has finished thinking.\n", p.name)
}

// dine simulates a philosopher dining.
func (p *Philosopher) dine() {
	for {
		p.think()
		p.leftFork.Lock()
		p.rightFork.Lock()
		p.eat()
		p.leftFork.Unlock()
		p.rightFork.Unlock()
	}
}

func main() {
	// Create chopsticks.
	chopsticks := make([]*Chopstick, 5)
	for i := range chopsticks {
		chopsticks[i] = &Chopstick{}
	}

	// Create philosophers.
	philosophers := make([]*Philosopher, 5)
	for i := range philosophers {
		philosophers[i] = createPhilosopher(fmt.Sprintf("Philosopher %d", i+1), chopsticks[i], chopsticks[(i+1)%5])
	}

	// Start philosophers dining.
	for _, p := range philosophers {
		go p.dine()
	}

	// Wait for philosophers to finish dining.
	time.Sleep(time.Second * 10)

	// Print the number of meals eaten by each philosopher.
	for _, p := range philosophers {
		fmt.Printf("%s has eaten %d meals.\n", p.name, p.mealsEaten)
	}
}
```

This code simulates the famous "Dining Philosophers" problem, where five philosophers sit at a circular table, each with a chopstick to their left and right. In order to eat, a philosopher needs to pick up both chopsticks, but only one philosopher can hold a chopstick at a time. The code uses goroutines and channels to simulate the philosophers dining, and it prints the number of meals eaten by each philosopher.