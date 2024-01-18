```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Define a type to represent a deck of cards
type deck []int

// Create a new deck of cards
func newDeck() deck {
	// Create a slice of integers to represent the deck of cards
	cards := make(deck, 52)

	// Loop through the cards and assign each card a value from 1 to 52
	for i := 0; i < len(cards); i++ {
		cards[i] = i + 1
	}

	// Return the deck of cards
	return cards
}

// Shuffle the deck of cards
func shuffleDeck(d deck) deck {
	// Create a new deck of cards to store the shuffled deck
	shuffledDeck := make(deck, len(d))

	// Create a random number generator
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	// Loop through the deck of cards and randomly select a card to add to the shuffled deck
	for i := 0; i < len(d); i++ {
		// Generate a random index between 0 and the length of the deck
		index := r.Intn(len(d))

		// Add the card at the random index to the shuffled deck
		shuffledDeck[i] = d[index]

		// Remove the card from the original deck
		d = append(d[:index], d[index+1:]...)
	}

	// Return the shuffled deck
	return shuffledDeck
}

func main() {
	// Create a new deck of cards
	deckOfCards := newDeck()

	// Print the original deck of cards
	fmt.Println("Original deck of cards:")
	fmt.Println(deckOfCards)

	// Shuffle the deck of cards
	shuffledDeck := shuffleDeck(deckOfCards)

	// Print the shuffled deck of cards
	fmt.Println("Shuffled deck of cards:")
	fmt.Println(shuffledDeck)
}
```

This code simulates a deck of cards and shuffles it.

It uses a slice to represent the deck of cards, which is an array of integers. The values of the integers represent the cards in the deck, with 1 representing the ace and 13 representing the king.

The function `newDeck()` creates a new deck of cards and returns it.

The function `shuffleDeck()` takes a deck of cards as input and returns a shuffled deck. It uses a random number generator to select a random card from the deck and adds it to the shuffled deck. It then removes the card from the original deck. This process is repeated until all of the cards have been shuffled.

The function `main()` creates a new deck of cards and prints it. It then shuffles the deck of cards and prints it again.

This code demonstrates the use of slices, random number generators, and functions in Go. It also provides an example of a real-world application of Go, such as simulating a deck of cards.