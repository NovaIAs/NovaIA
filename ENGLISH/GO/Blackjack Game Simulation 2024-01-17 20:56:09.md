```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Define a struct to represent a playing card
type Card struct {
	Suit  string  // The suit of the card (e.g. "Hearts")
	Rank  string  // The rank of the card (e.g. "Ace")
	Value int     // The numerical value of the card (e.g. 1 for Ace)
}

// Create a deck of cards
func createDeck() []Card {
	// Initialize an empty deck of cards
	deck := []Card{}
	
	// Create a list of suits and ranks
	suits := []string{"Hearts", "Diamonds", "Clubs", "Spades"}
	ranks := []string{"Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"}
	
	// Loop through each suit and rank to create each card
	for _, suit := range suits {
		for _, rank := range ranks {
			// Create a new card and assign its suit, rank, and value
			card := Card{Suit: suit, Rank: rank, Value: rankToInt(rank)}
			
			// Add the card to the deck
			deck = append(deck, card)
		}
	}
	
	// Return the deck of cards
	return deck
}

// Convert a rank string to an integer value
func rankToInt(rank string) int {
	switch rank {
	case "Ace":
		return 1
	case "2":
		return 2
	case "3":
		return 3
	case "4":
		return 4
	case "5":
		return 5
	case "6":
		return 6
	case "7":
		return 7
	case "8":
		return 8
	case "9":
		return 9
	case "10":
		return 10
	case "Jack":
		return 11
	case "Queen":
		return 12
	case "King":
		return 13
	default:
		return 0
	}
}

// Shuffle the deck of cards
func shuffleDeck(deck []Card) []Card {
	// Create a new random number generator
	rng := rand.New(rand.NewSource(time.Now().UnixNano()))
	
	// Loop through the deck and swap each card with a random card
	for i := range deck {
		j := rng.Intn(i + 1)
		deck[i], deck[j] = deck[j], deck[i]
	}
	
	// Return the shuffled deck
	return deck
}

// Deal a hand of cards to a player
func dealHand(deck []Card, numCards int) []Card {
	// Create a new hand of cards
	hand := []Card{}
	
	// Loop through the specified number of cards and add them to the hand
	for i := 0; i < numCards; i++ {
		// Remove the top card from the deck and add it to the hand
		hand = append(hand, deck[0])
		deck = deck[1:]
	}
	
	// Return the hand of cards
	return hand
}

// Calculate the score of a hand of cards
func calculateScore(hand []Card) int {
	// Initialize the score to 0
	score := 0
	
	// Loop through the cards in the hand and add their values to the score
	for _, card := range hand {
		score += card.Value
	}
	
	// Return the score
	return score
}

// Play a game of blackjack
func playBlackjack() {
	// Create a new deck of cards and shuffle it
	deck := createDeck()
	deck = shuffleDeck(deck)
	
	// Deal two cards to the player and two cards to the dealer
	playerHand := dealHand(deck, 2)
	dealerHand := dealHand(deck, 2)
	
	// Print the player's hand and the dealer's first card
	fmt.Println("Your hand:")
	for _, card := range playerHand {
		fmt.Printf("%s of %s\n", card.Rank, card.Suit)
	}
	fmt.Println("Dealer's hand:")
	fmt.Printf("%s of %s\n", dealerHand[0].Rank, dealerHand[0].Suit)
	fmt.Println("? of ?")
	
	// Prompt the player to hit or stand
	var input string
	fmt.Print("Hit or stand? (h/s) ")
	fmt.Scanln(&input)
	
	// Loop while the player wants to hit
	for input == "h" {
		// Deal a new card to the player
		playerHand = append(playerHand, deck[0])
		deck = deck[1:]
		
		// Print the player's hand
		fmt.Println("Your hand:")
		for _, card := range playerHand {
			fmt.Printf("%s of %s\n", card.Rank, card.Suit)
		}
		
		// Check if the player has busted
		if calculateScore(playerHand) > 21 {
			fmt.Println("You busted!")
			return
		}
		
		// Prompt the player to hit or stand
		fmt.Print("Hit or stand? (h/s) ")
		fmt.Scanln(&input)
	}
	
	// Reveal the dealer's second card
	fmt.Println("Dealer's hand:")
	for _, card := range dealerHand {
		fmt.Printf("%s of %s\n", card.Rank, card.Suit)
	}
	
	// Loop while the dealer needs to hit
	for calculateScore(dealerHand) < 17 {
		// Deal a new card to the dealer
		dealerHand = append(dealerHand, deck[0])
		deck = deck[1:]
		
		// Print the dealer's hand
		fmt.Println("Dealer's hand:")
		for _, card := range dealerHand {
			fmt.Printf("%s of %s\n", card.Rank, card.Suit)
		}
		
		// Check if the dealer has busted
		if calculateScore(dealerHand) > 21 {
			fmt.Println("Dealer busted!")
			return
		}
	}
	
	// Determine the winner
	playerScore := calculateScore(playerHand)
	dealerScore := calculateScore(dealerHand)
	
	if playerScore > dealerScore {
		fmt.Println("You win!")
	} else if playerScore == dealerScore {
		fmt.Println("Tie!")
	} else {
		fmt.Println("Dealer wins!")
	}
}

func main() {
	// Play a game of blackjack
	playBlackjack()
}
```

This code simulates a game of blackjack in Go. It creates a deck of cards, shuffles it, deals hands to the player and dealer, and allows the player to hit or stand. It also reveals the dealer's second card and determines the winner. The code includes functions to create the deck, shuffle the deck, deal hands, calculate the score of a hand, and play a game of blackjack.