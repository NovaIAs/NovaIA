```go
package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"math/rand"
	"sync"
	"time"
)

// Philosopher represents a philosopher.
type Philosopher struct {
	id             int
	leftChopstick  *Chopstick
	rightChopstick *Chopstick
}

// Chopstick represents a chopstick.
type Chopstick struct {
	id int
}

// Table represents a table.
type Table struct {
	philosophers []*Philosopher
	chopsticks   []*Chopstick
}

// Start starts the simulation.
func (t *Table) Start(ctx context.Context) {
	for _, p := range t.philosophers {
		go p.Dine(ctx)
	}
}

// Dine represents a philosopher dining.
func (p *Philosopher) Dine(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		default:
			p.Think()
			p.PickUpChopsticks()
			p.Eat()
			p.PutDownChopsticks()
		}
	}
}

// Think represents a philosopher thinking.
func (p *Philosopher) Think() {
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
}

// PickUpChopsticks represents a philosopher picking up chopsticks.
func (p *Philosopher) PickUpChopsticks() {
	p.leftChopstick.Lock()
	p.rightChopstick.Lock()
}

// Eat represents a philosopher eating.
func (p *Philosopher) Eat() {
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
}

// PutDownChopsticks represents a philosopher putting down chopsticks.
func (p *Philosopher) PutDownChopsticks() {
	p.leftChopstick.Unlock()
	p.rightChopstick.Unlock()
}

// NewTable creates a new table.
func NewTable(numPhilosophers int) *Table {
	t := &Table{
		philosophers: make([]*Philosopher, numPhilosophers),
		chopsticks:   make([]*Chopstick, numPhilosophers),
	}

	for i := 0; i < numPhilosophers; i++ {
		t.philosophers[i] = &Philosopher{
			id:             i,
			leftChopstick:  t.chopsticks[i],
			rightChopstick: t.chopsticks[(i+1)%numPhilosophers],
		}
	}

	return t
}

func main() {
	t := NewTable(5)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go t.Start(ctx)

	time.Sleep(30 * time.Second)
	cancel()

	fmt.Println("The philosophers have finished dining.")
}

// Chopstick represents a chopstick.
type Chopstick struct {
	id    int
	mutex *sync.Mutex
}

// NewChopstick creates a new chopstick.
func NewChopstick(id int) *Chopstick {
	return &Chopstick{
		id:    id,
		mutex: &sync.Mutex{},
	}
}

// Lock locks the chopstick.
func (c *Chopstick) Lock() {
	c.mutex.Lock()
}

// Unlock unlocks the chopstick.
func (c *Chopstick) Unlock() {
	c.mutex.Unlock()
}

// Philosopher represents a philosopher.
type Philosopher struct {
	id              int
	leftChopstick   *Chopstick
	rightChopstick  *Chopstick
	numMealsEaten  int
	logger          *log.Logger
	eatingDuration time.Duration
}

// NewPhilosopher creates a new philosopher.
func NewPhilosopher(id int, leftChopstick, rightChopstick *Chopstick, logger *log.Logger) *Philosopher {
	return &Philosopher{
		id:              id,
		leftChopstick:   leftChopstick,
		rightChopstick:  rightChopstick,
		logger:          logger,
		eatingDuration: time.Duration(rand.Intn(1000)) * time.Millisecond,
	}
}

// Dine represents a philosopher dining.
func (p *Philosopher) Dine() {
	p.logger.Printf("Philosopher %d is dining.", p.id)

	p.PickUpChopsticks()
	p.Eat()
	p.PutDownChopsticks()

	p.numMealsEaten++
}

// PickUpChopsticks represents a philosopher picking up chopsticks.
func (p *Philosopher) PickUpChopsticks() {
	p.logger.Printf("Philosopher %d is picking up chopsticks.", p.id)

	p.leftChopstick.Lock()
	p.rightChopstick.Lock()
}

// Eat represents a philosopher eating.
func (p *Philosopher) Eat() {
	p.logger.Printf("Philosopher %d is eating.", p.id)

	time.Sleep(p.eatingDuration)
}

// PutDownChopsticks represents a philosopher putting down chopsticks.
func (p *Philosopher) PutDownChopsticks() {
	p.logger.Printf("Philosopher %d is putting down chopsticks.", p.id)

	p.leftChopstick.Unlock()
	p.rightChopstick.Unlock()
}

// Table represents a table.
type Table struct {
	philosophers []*Philosopher
	chopsticks   []*Chopstick
	logger       *log.Logger
}

// NewTable creates a new table.
func NewTable(numPhilosophers int, logger *log.Logger) *Table {
	t := &Table{
		philosophers: make([]*Philosopher, numPhilosophers),
		chopsticks:   make([]*Chopstick, numPhilosophers),
		logger:       logger,
	}

	for i := 0; i < numPhilosoph
```