```javascript
// Define a function to generate a random number within a specified range
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Create an array of objects to represent a deck of cards
const deck = [];
const suits = ["Hearts", "Diamonds", "Clubs", "Spades"];
const values = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"];

for (let i = 0; i < suits.length; i++) {
  for (let j = 0; j < values.length; j++) {
    deck.push({ suit: suits[i], value: values[j] });
  }
}

// Shuffle the deck of cards using the Fisher-Yates shuffle algorithm
function shuffleDeck(deck) {
  for (let i = deck.length - 1; i > 0; i--) {
    const j = getRandomNumber(0, i);
    const temp = deck[i];
    deck[i] = deck[j];
    deck[j] = temp;
  }
}

// Create a function to deal a hand of cards from the deck
function dealHand(deck, numCards) {
  const hand = [];
  for (let i = 0; i < numCards; i++) {
    hand.push(deck.pop());
  }
  return hand;
}

// Create a function to calculate the value of a hand of cards
function calculateHandValue(hand) {
  let value = 0;
  let hasAce = false;

  for (let i = 0; i < hand.length; i++) {
    const card = hand[i];
    if (card.value === "Ace") {
      hasAce = true;
    } else if (card.value === "Jack" || card.value === "Queen" || card.value === "King") {
      value += 10;
    } else {
      value += parseInt(card.value);
    }
  }

  // If the hand contains an ace, add 10 to the value if it doesn't bust
  if (hasAce && value + 10 <= 21) {
    value += 10;
  }

  return value;
}

// Create a function to determine if a hand is a blackjack
function isBlackjack(hand) {
  return hand.length === 2 && calculateHandValue(hand) === 21;
}

// Create a function to determine if a hand is a bust
function isBust(hand) {
  return calculateHandValue(hand) > 21;
}

// Create a function to play a game of blackjack
function playBlackjack() {
  // Initialize the game state
  let gameOver = false;
  let playerHand = [];
  let dealerHand = [];

  // Deal the initial hands
  playerHand = dealHand(deck, 2);
  dealerHand = dealHand(deck, 2);

  // Check if either player has a blackjack
  if (isBlackjack(playerHand) || isBlackjack(dealerHand)) {
    gameOver = true;
  }

  // If no one has a blackjack, start the game loop
  while (!gameOver) {
    // Player's turn
    let playerStand = false;
    while (!playerStand) {
      // Prompt the player to hit or stand
      const playerChoice = prompt("Hit (h) or Stand (s)?");

      // If the player hits, deal another card
      if (playerChoice === "h") {
        playerHand.push(deck.pop());

        // Check if the player busted
        if (isBust(playerHand)) {
          alert("Player busted!");
          gameOver = true;
          break;
        }
      } else if (playerChoice === "s") {
        playerStand = true;
      }
    }

    // Dealer's turn
    while (!gameOver) {
      // Check if the dealer has a blackjack
      if (isBlackjack(dealerHand)) {
        alert("Dealer has a blackjack!");
        gameOver = true;
        break;
      }

      // If the dealer doesn't have a blackjack, deal another card if the dealer's hand value is less than 17
      if (calculateHandValue(dealerHand) < 17) {
        dealerHand.push(deck.pop());
      } else {
        // Otherwise, the dealer stands
        break;
      }

      // Check if the dealer busted
      if (isBust(dealerHand)) {
        alert("Dealer busted!");
        gameOver = true;
        break;
      }
    }

    // Determine the winner
    if (gameOver) {
      if (isBlackjack(playerHand) && isBlackjack(dealerHand)) {
        alert("Push!");
      } else if (isBlackjack(playerHand)) {
        alert("Player has a blackjack and wins!");
      } else if (isBlackjack(dealerHand)) {
        alert("Dealer has a blackjack and wins!");
      } else if (calculateHandValue(playerHand) > calculateHandValue(dealerHand)) {
        alert("Player wins!");
      } else if (calculateHandValue(playerHand) < calculateHandValue(dealerHand)) {
        alert("Dealer wins!");
      } else {
        alert("Push!");
      }
    }
  }
}

// Shuffle the deck and start a new game of blackjack
shuffleDeck(deck);
playBlackjack();
```

Explanation:

This JavaScript code simulates a game of blackjack between a player and a dealer. It includes a deck of cards, shuffling, dealing, calculating hand values, and determining blackjacks and busts. It also handles the game loop and determines the winner.

1. **Deck of Cards:**
   - The `deck` array represents a standard deck of 52 cards with four suits (Hearts, Diamonds, Clubs, Spades) and 13 values (2 through Ace).
   - It is created by looping through the suits and values and pushing each card object into the array.

2. **Shuffling the Deck:**
   - The `shuffleDeck` function uses the Fisher-Yates shuffle algorithm to randomly rearrange the cards in the deck.

3. **Dealing Hands:**
   - The `dealHand` function takes the deck and a number of cards as arguments and deals that many cards from the top of the deck.

4. **Calculating Hand Value:**
   - The `calculateHandValue` function takes a hand of cards as an argument and calculates its value according to blackjack rules.
   - It handles aces correctly, adding 10 to the value if it doesn't cause a bust.

5. **Blackjack and Bust Checks:**
   - The `isBlackjack` and `isBust` functions check if a hand is a blackjack (21 with two cards) or a bust (over 21), respectively.

6. **Game Loop:**
   - The `playBlackjack` function runs the game loop, which includes the player's and dealer's turns.
   - The player can hit (draw another card) or stand (end their turn), while the dealer draws cards until their hand value reaches 17 or they bust.

7. **Determining the Winner:**
   - After both the player and dealer have completed their turns, the game checks for blackjacks, busts, and compares hand values to determine the winner or a push (tie).

8. **Starting a New Game:**
   - The game is played by first shuffling the deck and then calling the `playBlackjack` function.