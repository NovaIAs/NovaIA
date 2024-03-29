```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// Define a struct to represent a playing card
type Card struct {
    suit string
    rank string
}

// Define a slice of strings to represent the suits of the cards
var suits = []string{"Hearts", "Diamonds", "Clubs", "Spades"}

// Define a slice of strings to represent the ranks of the cards
var ranks = []string{"2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"}

// Define a function to generate a random card
func generateCard() Card {
    // Generate a random suit
    suit := suits[rand.Intn(len(suits))]

    // Generate a random rank
    rank := ranks[rand.Intn(len(ranks))]

    // Return a new Card struct with the generated suit and rank
    return Card{suit: suit, rank: rank}
}

// Define a function to generate a deck of cards
func generateDeck() []Card {
    // Create a new slice of cards to store the deck
    deck := make([]Card, len(suits)*len(ranks))

    // Iterate over the suits and ranks to generate each card in the deck
    for i, suit := range suits {
        for j, rank := range ranks {
            deck[i*len(ranks)+j] = Card{suit: suit, rank: rank}
        }
    }

    // Return the generated deck
    return deck
}

// Define a function to shuffle a deck of cards
func shuffleDeck(deck []Card) {
    // Use the Fisher-Yates shuffle algorithm to shuffle the deck
    for i := len(deck) - 1; i > 0; i-- {
        j := rand.Intn(i + 1)
        deck[i], deck[j] = deck[j], deck[i]
    }
}

// Define a function to deal a hand of cards to a player
func dealHand(deck []Card, numCards int) []Card {
    // Create a new slice of cards to store the hand
    hand := make([]Card, numCards)

    // Deal the cards from the deck to the hand
    for i := 0; i < numCards; i++ {
        hand[i] = deck[i]
    }

    // Return the hand
    return hand
}

// Define a function to print a hand of cards
func printHand(hand []Card) {
    // Iterate over the cards in the hand and print them
    for _, card := range hand {
        fmt.Println(card.rank, "of", card.suit)
    }
}

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())

    // Generate a deck of cards
    deck := generateDeck()

    // Shuffle the deck
    shuffleDeck(deck)

    // Deal a hand of 5 cards to the player
    hand := dealHand(deck, 5)

    // Print the player's hand
    fmt.Println("Your hand:")
    printHand(hand)
}
```

This code simulates a card game where a player is dealt a hand of 5 cards from a standard deck of 52 cards. Here's a detailed explanation of the code:

1. **Card Struct**:
   - We define a struct called `Card` to represent a single playing card. It has two fields: `suit` (which can be "Hearts", "Diamonds", "Clubs", or "Spades") and `rank` (which can be "2" to "Ace").

2. **Suits and Ranks**:
   - We define two string slices, `suits` and `ranks`, which contain the possible suits and ranks of cards in a standard deck.

3. **Generate Random Card**:
   - The `generateCard()` function generates a random card by randomly selecting a suit from `suits` and a rank from `ranks`. It returns a new `Card` struct with the generated suit and rank.

4. **Generate Deck**:
   - The `generateDeck()` function generates a complete deck of 52 cards by iterating over all possible combinations of suits and ranks. It creates a slice of `Card` structs and populates it with cards generated by `generateCard()`.

5. **Shuffle Deck**:
   - The `shuffleDeck()` function shuffles the deck of cards using the Fisher-Yates shuffle algorithm. It randomly swaps cards in the deck to create a randomized order.

6. **Deal Hand**:
   - The `dealHand()` function deals a hand of a specified number of cards to a player. It takes the deck of cards and the number of cards to deal as arguments. It creates a slice of `Card` structs and populates it with cards from the deck, starting from the top.

7. **Print Hand**:
   - The `printHand()` function prints the cards in a hand. It iterates over the cards in the hand and prints the rank and suit of each card.

8. **Main Function**:
   - In the `main()` function:
     - We seed the random number generator with the current time to ensure different results each time the program runs.
     - We generate a deck of cards using `generateDeck()` and shuffle it using `shuffleDeck()`.
     - We deal a hand of 5 cards to the player using `dealHand()` and print the hand using `printHand()`.