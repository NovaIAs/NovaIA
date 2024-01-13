```javascript
// Function to generate a random integer between min and max
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Function to shuffle an array
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
}

// Function to create a deck of cards
function createDeck() {
  const suits = ["Hearts", "Diamonds", "Clubs", "Spades"];
  const ranks = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"];
  const deck = [];
  for (let suit of suits) {
    for (let rank of ranks) {
      deck.push({ suit, rank });
    }
  }
  return deck;
}

// Function to deal a hand of cards
function dealHand(deck, numCards) {
  const hand = [];
  for (let i = 0; i < numCards; i++) {
    const randomIndex = getRandomInt(0, deck.length - 1);
    hand.push(deck.splice(randomIndex, 1)[0]);
  }
  return hand;
}

// Function to calculate the score of a hand
function calculateScore(hand) {
  let score = 0;
  for (let card of hand) {
    if (card.rank === "Ace") {
      score += 11;
    } else if (card.rank === "Jack" || card.rank === "Queen" || card.rank === "King") {
      score += 10;
    } else {
      score += parseInt(card.rank);
    }
  }
  return score;
}

// Function to determine the winner of a hand
function determineWinner(playerHand, dealerHand) {
  const playerScore = calculateScore(playerHand);
  const dealerScore = calculateScore(dealerHand);

  if (playerScore > 21 && dealerScore > 21) {
    return "Tie";
  } else if (playerScore > 21) {
    return "Dealer Wins";
  } else if (dealerScore > 21) {
    return "Player Wins";
  } else if (playerScore === 21 && dealerScore === 21) {
    return "Tie";
  } else if (playerScore === 21) {
    return "Player Wins";
  } else if (dealerScore === 21) {
    return "Dealer Wins";
  } else if (playerScore > dealerScore) {
    return "Player Wins";
  } else if (dealerScore > playerScore) {
    return "Dealer Wins";
  } else {
    return "Tie";
  }
}

// Function to play a round of blackjack
function playRound() {
  const deck = createDeck();
  shuffleArray(deck);
  const playerHand = dealHand(deck, 2);
  const dealerHand = dealHand(deck, 2);

  console.log("Player Hand:", playerHand);
  console.log("Dealer Hand:", dealerHand);

  let playerStand = false;
  let dealerStand = false;

  while (!playerStand || !dealerStand) {
    if (!playerStand) {
      const playerAction = prompt("Hit or Stand?");
      if (playerAction === "Hit") {
        playerHand.push(dealHand(deck, 1)[0]);
        console.log("Player Hand:", playerHand);
        const playerScore = calculateScore(playerHand);
        if (playerScore > 21) {
          console.log("Player Bust!");
          playerStand = true;
        }
      } else {
        playerStand = true;
      }
    }

    if (!dealerStand) {
      const dealerScore = calculateScore(dealerHand);
      if (dealerScore < 17) {
        dealerHand.push(dealHand(deck, 1)[0]);
        console.log("Dealer Hand:", dealerHand);
      } else {
        dealerStand = true;
      }
    }
  }

  const winner = determineWinner(playerHand, dealerHand);
  console.log("Winner:", winner);
}

// Play multiple rounds of blackjack
for (let i = 0; i < 10; i++) {
  playRound();
}
```

This code simulates a game of blackjack in JavaScript. Here's a breakdown:

1. **Function getRandomInt:** Generates a random integer between a minimum and maximum value.

2. **Function shuffleArray:** Shuffles an array randomly.

3. **Function createDeck:** Creates a standard deck of 52 playing cards.

4. **Function dealHand:** Deals a hand of cards from a deck.

5. **Function calculateScore:** Calculates the score of a hand of cards.

6. **Function determineWinner:** Determines the winner of a hand of blackjack based on the scores.

7. **Function playRound:** Simulates a single round of blackjack, including player and dealer turns.

8. **Main Loop:** Plays multiple rounds of blackjack (10 rounds in this case).

This code provides a detailed simulation of blackjack, including dealing cards, calculating scores, determining the winner, and playing multiple rounds. It demonstrates various JavaScript concepts such as functions, loops, arrays, and object manipulation.