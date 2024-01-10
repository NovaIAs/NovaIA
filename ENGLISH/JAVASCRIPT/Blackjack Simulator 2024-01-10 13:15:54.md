```javascript
// Create a function to generate a random number between two values (inclusive)
const randomInt = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

// Create a class to represent a playing card
class Card {
  constructor(suit, rank) {
    this.suit = suit;
    this.rank = rank;
  }

  // Return the string representation of the card
  toString() {
    return `${this.rank} of ${this.suit}`;
  }
}

// Create a function to generate a standard deck of 52 cards
const generateDeck = () => {
  const suits = ["Hearts", "Diamonds", "Clubs", "Spades"];
  const ranks = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"];
  const deck = [];

  for (let i = 0; i < suits.length; i++) {
    for (let j = 0; j < ranks.length; j++) {
      deck.push(new Card(suits[i], ranks[j]));
    }
  }

  return deck;
};

// Create a function to shuffle a deck of cards
const shuffleDeck = (deck) => {
  for (let i = 0; i < deck.length; i++) {
    const randomIndex = randomInt(0, deck.length - 1);
    const temp = deck[i];
    deck[i] = deck[randomIndex];
    deck[randomIndex] = temp;
  }

  return deck;
};

// Create a function to deal a hand of cards to a player
const dealHand = (deck, numCards) => {
  const hand = [];
  for (let i = 0; i < numCards; i++) {
    hand.push(deck.pop());
  }

  return hand;
};

// Create a function to calculate the score of a hand of cards
const calculateScore = (hand) => {
  let score = 0;
  for (let i = 0; i < hand.length; i++) {
    const card = hand[i];
    const rank = card.rank;
    if (rank === "Ace") {
      score += 11;
    } else if (rank === "Jack" || rank === "Queen" || rank === "King") {
      score += 10;
    } else {
      score += parseInt(rank);
    }
  }

  // Adjust the score for aces if necessary
  for (let i = 0; i < hand.length; i++) {
    const card = hand[i];
    if (card.rank === "Ace" && score > 21) {
      score -= 10;
    }
  }

  return score;
};

// Create a function to play a game of blackjack
const playBlackjack = () => {
  // Create a deck of cards and shuffle it
  const deck = generateDeck();
  shuffleDeck(deck);

  // Deal two cards to the player and two cards to the dealer
  const playerHand = dealHand(deck, 2);
  const dealerHand = dealHand(deck, 2);

  // Display the player's hand and the dealer's first card
  console.log("Player's Hand:");
  console.log(playerHand.join(", "));
  console.log("Dealer's Hand:");
  console.log(dealerHand[0].toString());

  // Get the player's decision (hit or stand)
  let playerDecision = prompt("Hit or Stand? (h/s)");

  // While the player wants to hit, deal them another card and display their hand
  while (playerDecision === "h") {
    playerHand.push(deck.pop());
    console.log("Player's Hand:");
    console.log(playerHand.join(", "));
    playerDecision = prompt("Hit or Stand? (h/s)");
  }

  // Calculate the player's score
  const playerScore = calculateScore(playerHand);

  // If the player's score is over 21, they bust and lose
  if (playerScore > 21) {
    console.log("Player Busts!");
    return;
  }

  // Display the dealer's hand
  console.log("Dealer's Hand:");
  console.log(dealerHand.join(", "));

  // While the dealer's score is less than 17, deal them another card and display their hand
  while (calculateScore(dealerHand) < 17) {
    dealerHand.push(deck.pop());
    console.log("Dealer's Hand:");
    console.log(dealerHand.join(", "));
  }

  // Calculate the dealer's score
  const dealerScore = calculateScore(dealerHand);

  // Determine the winner
  if (dealerScore > 21) {
    console.log("Dealer Busts!");
  } else if (playerScore > dealerScore) {
    console.log("Player Wins!");
  } else if (playerScore === dealerScore) {
    console.log("Tie!");
  } else {
    console.log("Dealer Wins!");
  }
};

// Play the game of blackjack
playBlackjack();
```

This code simulates a game of blackjack in JavaScript. It includes functions to generate a deck of cards, shuffle the deck, deal cards to players, calculate the score of a hand, and play a game of blackjack. The game logic is implemented using nested loops and conditional statements to determine the winner based on the rules of blackjack. The code is well-commented and uses helper functions to keep the code organized and readable.