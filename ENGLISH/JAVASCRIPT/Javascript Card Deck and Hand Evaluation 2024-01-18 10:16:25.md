```javascript
// Define a function to generate a random string of a given length.
function generateRandomString(length) {
  // Create an empty string to store the random string.
  let randomString = "";

  // Loop through the desired length of the string.
  for (let i = 0; i < length; i++) {
    // Generate a random number between 0 and 9.
    let randomNumber = Math.floor(Math.random() * 10);

    // Convert the random number to a string.
    let randomStringCharacter = randomNumber.toString();

    // Add the random string character to the random string.
    randomString += randomStringCharacter;
  }

  // Return the generated random string.
  return randomString;
}

// Define a function to shuffle an array of elements.
function shuffleArray(array) {
  // Create a new array to store the shuffled elements.
  let shuffledArray = [];

  // While there are still elements in the original array.
  while (array.length > 0) {
    // Generate a random index within the original array.
    let randomIndex = Math.floor(Math.random() * array.length);

    // Remove the element at the random index from the original array.
    let removedElement = array.splice(randomIndex, 1);

    // Add the removed element to the shuffled array.
    shuffledArray.push(removedElement[0]);
  }

  // Return the shuffled array.
  return shuffledArray;
}

// Define a function to create a new deck of cards.
function createDeckOfCards() {
  // Create an empty array to store the deck of cards.
  let deckOfCards = [];

  // Create an array of card suits.
  let suits = ["Hearts", "Diamonds", "Clubs", "Spades"];

  // Create an array of card values.
  let values = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"];

  // Loop through the suits and values to create each card.
  for (let suit of suits) {
    for (let value of values) {
      // Create a new card object.
      let card = {
        suit: suit,
        value: value
      };

      // Add the card to the deck of cards.
      deckOfCards.push(card);
    }
  }

  // Return the deck of cards.
  return deckOfCards;
}

// Define a function to deal a hand of cards from a deck.
function dealHandOfCards(deckOfCards, numberOfCards) {
  // Create an empty array to store the hand of cards.
  let handOfCards = [];

  // Loop through the desired number of cards.
  for (let i = 0; i < numberOfCards; i++) {
    // Generate a random index within the deck of cards.
    let randomIndex = Math.floor(Math.random() * deckOfCards.length);

    // Remove the card at the random index from the deck of cards.
    let dealtCard = deckOfCards.splice(randomIndex, 1);

    // Add the dealt card to the hand of cards.
    handOfCards.push(dealtCard[0]);
  }

  // Return the hand of cards.
  return handOfCards;
}

// Define a function to evaluate a hand of cards.
function evaluateHandOfCards(handOfCards) {
  // Create an object to store the evaluation of the hand of cards.
  let handEvaluation = {
    highestCard: "",
    highestCardValue: 0,
    pair: false,
    twoPair: false,
    threeOfAKind: false,
    fullHouse: false,
    fourOfAKind: false,
    straight: false,
    flush: false,
    straightFlush: false,
    royalFlush: false
  };

  // Sort the hand of cards by value in descending order.
  handOfCards.sort((a, b) => {
    return b.value - a.value;
  });

  // Check for a royal flush.
  if (
    handOfCards[0].value === "Ace" &&
    handOfCards[1].value === "King" &&
    handOfCards[2].value === "Queen" &&
    handOfCards[3].value === "Jack" &&
    handOfCards[4].value === "10" &&
    handOfCards[0].suit === handOfCards[1].suit &&
    handOfCards[1].suit === handOfCards[2].suit &&
    handOfCards[2].suit === handOfCards[3].suit &&
    handOfCards[3].suit === handOfCards[4].suit
  ) {
    handEvaluation.royalFlush = true;
    return handEvaluation;
  }

  // Check for a straight flush.
  if (
    handOfCards[0].value - handOfCards[1].