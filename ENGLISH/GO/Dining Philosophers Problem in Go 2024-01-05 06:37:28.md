```go
package main

import (
	"context"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Philosopher represents a philosopher.
type Philosopher struct {
	id          int
	leftChopstick *chopstick
	rightChopstick *chopstick
}

// chopstick represents a chopstick.
type chopstick struct {
	m sync.Mutex
}

// eat simulates a philosopher eating.
func (p *Philosopher) eat(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		default:
			p.leftChopstick.m.Lock()
			p.rightChopstick.m.Lock()

			fmt.Printf("Philosopher %d is eating.\n", p.id)
			time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)

			p.rightChopstick.m.Unlock()
			p.leftChopstick.m.Unlock()
		}
	}
}

func main() {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	chopsticks := make([]*chopstick, 5)
	for i := range chopsticks {
		chopsticks[i] = &chopstick{}
	}

	philosophers := make([]*Philosopher, 5)
	for i := range philosophers {
		philosophers[i] = &Philosopher{
			id:              i + 1,
			leftChopstick: chopsticks[i],
			rightChopstick: chopsticks[(i+1)%len(chopsticks)],
		}
	}

	for _, p := range philosophers {
		go p.eat(ctx)
	}

	<-ctx.Done()
}
```

This code simulates the famous "dining philosophers" problem using Go. In this problem, there are five philosophers sitting around a circular table, with a bowl of rice and a pair of chopsticks in front of each philosopher. The philosophers can only eat if they have both chopsticks, but they can only pick up one chopstick at a time. If a philosopher picks up one chopstick and then tries to pick up the other chopstick, they will have to wait until the other philosopher who has the other chopstick puts it down.

The code uses a `context.Context` to simulate the fact that the philosophers can only eat for a limited amount of time. The `context.Context` is passed to the `eat` method of the `Philosopher` struct, and the `eat` method uses a `select` statement to wait for either the `context.Context` to be canceled or for the philosopher to be able to eat.

The `eat` method uses two `sync.Mutex`es to simulate the chopsticks. The `sync.Mutex`es are locked when a philosopher picks up a chopstick, and they are unlocked when the philosopher puts the chopstick down. This ensures that only one philosopher can use a chopstick at a time.

The main function creates the chopsticks, the philosophers, and the `context.Context`. It then starts a goroutine for each philosopher, and it waits for the `context.Context` to be canceled before exiting.

This code is complex because it uses a number of different concurrency primitives, including `context.Context`, `sync.Mutex`, and goroutines. It also uses a `select` statement to wait for multiple events to occur.