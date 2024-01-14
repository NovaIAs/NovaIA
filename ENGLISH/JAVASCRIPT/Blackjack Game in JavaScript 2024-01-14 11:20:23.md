```javascript
// Define an object to represent a playing card
class PlayingCard {
  constructor(suit, rank) {
    this.suit = suit;
    this.rank = rank;
  }

  // Get the value of the card, where Ace is 1, Jack is 11, Queen is 12, and King is 13
  getValue() {
    if (this.rank === "Ace") {
      return 1;
    } else if (this.rank === "Jack" || this.rank === "Queen" || this.rank === "King") {
      return 10;
    } else {
      return parseInt(this.rank);
    }
  }

  // Get the string representation of the card, e.g. "Ace of Spades"
  toString() {
    return `${this.rank} of ${this.suit}`;
  }
}

// Define an array of all possible suits and ranks for a standard deck of playing cards
const suits = ["Hearts", "Diamonds", "Clubs", "Spades"];
const ranks = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"];

// Create a function to generate a standard deck of playing cards
function generateDeck() {
  const deck = [];
  for (let i = 0; i < suits.length; i++) {
    for (let j = 0; j < ranks.length; j++) {
      deck.push(new PlayingCard(suits[i], ranks[j]));
    }
  }
  return deck;
}

// Create a function to shuffle a deck of playing cards
function shuffleDeck(deck) {
  for (let i = deck.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [deck[i], deck[j]] = [deck[j], deck[i]];
  }
}

// Create a function to deal a hand of playing cards from a deck
function dealHand(deck, numCards) {
  const hand = [];
  for (let i = 0; i < numCards; i++) {
    hand.push(deck.pop());
  }
  return hand;
}

// Create a function to calculate the value of a hand of playing cards
function calculateHandValue(hand) {
  let totalValue = 0;
  let hasAce = false;
  for (let i = 0; i < hand.length; i++) {
    const cardValue = hand[i].getValue();
    totalValue += cardValue;
    if (cardValue === 1) {
      hasAce = true;
    }
  }

  // If there is an Ace and the total value is less than 12, add 10 to the total value
  if (hasAce && totalValue <= 11) {
    totalValue += 10;
  }

  return totalValue;
}

// Create a function to play a round of blackjack
function playBlackjack(deck) {
  // Deal the initial hands to the player and the dealer
  const playerHand = dealHand(deck, 2);
  const dealerHand = dealHand(deck, 2);

  // Display the player's hand and the dealer's face-up card
  console.log("Player's Hand:");
  console.log(playerHand.toString());
  console.log("Dealer's Hand:");
  console.log(dealerHand[0].toString());

  // Check if the player has a blackjack
  if (calculateHandValue(playerHand) === 21) {
    console.log("Player has a blackjack!");
    return;
  }

  // Check if the dealer has a blackjack
  if (calculateHandValue(dealerHand) === 21) {
    console.log("Dealer has a blackjack!");
    return;
  }

  // Player's turn
  while (true) {
    // Prompt the player to hit or stand
    const playerChoice = prompt("Hit or Stand? (H/S)");

    // If the player hits, deal them another card
    if (playerChoice === "H") {
      playerHand.push(deck.pop());
      console.log("Player's Hand:");
      console.log(playerHand.toString());

      // Check if the player busted
      if (calculateHandValue(playerHand) > 21) {
        console.log("Player busted!");
        return;
      }
    } else if (playerChoice === "S") {
      break;
    } else {
      console.log("Invalid choice. Please enter 'H' to hit or 'S' to stand.");
    }
  }

  // Dealer's turn
  while (true) {
    // If the dealer's hand value is less than 17, they must hit
    if (calculateHandValue(dealerHand) < 17) {
      dealerHand.push(deck.pop());
      console.log("Dealer's Hand:");
      console.log(dealerHand.toString());

      // Check if the dealer busted
      if (calculateHandValue(dealerHand) > 21) {
        console.log("Dealer busted!");
        return;
      }
    } else {
      break;
    }
  }

  // Determine the winner
  const playerHandValue = calculateHandValue(playerHand);
  const dealerHandValue = calculateHandValue(dealerHand);
  if (playerHandValue > dealerHandValue) {
    console.log("Player wins!");
  } else if (playerHandValue < dealerHandValue) {
    console.log("Dealer wins!");
  } else {
    console.log("Push!");
  }
}

// Generate a deck of cards