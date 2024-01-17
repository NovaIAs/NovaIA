import Foundation
// Function to generate a random integer within a range
func randomInt(min: Int, max: Int) -> Int {
    return Int.random(in: min...max)
}
// Class to represent a deck of cards
class Deck {
    // Array of cards in the deck
    private var cards: [Card]
    // Initialize a new deck with a standard set of 52 cards
    init() {
        cards = []
        // Loop through suits and values to create all possible cards
        for suit in Suit.allCases {
            for value in CardValue.allCases {
                cards.append(Card(suit: suit, value: value))
            }
        }
    }
    // Shuffle the deck of cards
    func shuffle() {
        cards.shuffle()
    }
    // Deal a card from the deck
    func deal() -> Card? {
        guard !cards.isEmpty else { return nil }
        return cards.removeLast()
    }
}
// Class to represent a card
class Card {
    // Suit of the card
    let suit: Suit
    // Value of the card
    let value: CardValue
    // Initialize a new card
    init(suit: Suit, value: CardValue) {
        self.suit = suit
        self.value = value
    }
    // String representation of the card
    var description: String {
        return "\(value) of \(suit)"
    }
}
// Enum to represent the suits of a card
enum Suit: CaseIterable {
    case hearts
    case diamonds
    case clubs
    case spades
}
// Enum to represent the values of a card
enum CardValue: CaseIterable {
    case ace
    case two
    case three
    case four
    case five
    case six
    case seven
    case eight
    case nine
    case ten
    case jack
    case queen
    case king
}
// Function to play a game of blackjack
func playBlackjack() {
    // Create a new deck of cards and shuffle it
    let deck = Deck()
    deck.shuffle()
    // Create dealer and player hands
    var dealerHand: [Card] = []
    var playerHand: [Card] = []
    // Deal two cards to each hand
    dealerHand.append(deck.deal()!)
    dealerHand.append(deck.deal()!)
    playerHand.append(deck.deal()!)
    playerHand.append(deck.deal()!)
    // Display the dealer's hand, hiding one card
    print("Dealer's hand:")
    print("\(dealerHand[0])")
    print("[hidden]")
    // Display the player's hand
    print("Your hand:")
    for card in playerHand {
        print(card)
    }
    // Check if either player has blackjack
    if hasBlackjack(hand: dealerHand) {
        print("Dealer has blackjack! You lose.")
        return
    }
    if hasBlackjack(hand: playerHand) {
        print("You have blackjack! You win!")
        return
    }
    // Start the game loop
    while true {
        // Get player's choice
        print("What do you want to do? (hit/stand)")
        let input = readLine()!.lowercased()
        // Process player's choice
        switch input {
        case "hit":
            // Deal a card to the player's hand
            playerHand.append(deck.deal()!)
            // Check if the player has busted
            if getHandValue(hand: playerHand) > 21 {
                print("You busted! You lose.")
                return
            }
            // Display the player's hand
            print("Your hand:")
            for card in playerHand {
                print(card)
            }
        case "stand":
            // Move to the dealer's turn
            break
        default:
            print("Invalid input. Please enter 'hit' or 'stand'.")
        }
        // Dealer's turn
        while getHandValue(hand: dealerHand) < 17 {
            // Deal a card to the dealer's hand
            dealerHand.append(deck.deal()!)
            // Check if the dealer has busted
            if getHandValue(hand: dealerHand) > 21 {
                print("Dealer busted! You win!")
                return
            }
        }
        // Display the dealer's hand
        print("Dealer's hand:")
        for card in dealerHand {
            print(card)
        }
        // Compare the hands to determine the winner
        let dealerHandValue = getHandValue(hand: dealerHand)
        let playerHandValue = getHandValue(hand: playerHand)
        if dealerHandValue > playerHandValue {
            print("Dealer wins!")
        } else if dealerHandValue == playerHandValue {
            print("Tie!")
        } else {
            print("You win!")
        }
        return
    }
}
// Function to check if a hand has blackjack
func hasBlackjack(hand: [Card]) -> Bool {
    if hand.count != 2 { return false }
    if hand[0].value == .ace && hand[1].value == .ten { return true }
    if hand[0].value == .ten && hand[1].value == .ace { return true }
    return false
}
// Function to get the value of a hand
func getHandValue(hand: [Card]) -> Int {
    var total = 0
    for card in hand {
        switch card.value {
        case .ace:
            // Ace can be worth 1 or 11
            total += 11
            if total > 21 {
                total -= 10 // Ace is now worth 1
            }
        case .two:
            total += 2
        case .three:
            total += 3
        case .four:
            total += 4
        case .five:
            total += 5
        case .six:
            total += 6
        case .seven:
            total += 7
        case .eight:
            total += 8
        case .nine:
            total += 9
        case .ten, .jack, .queen, .king:
            total += 10
        }
    }
    return total
}
// Play the game
playBlackjack()