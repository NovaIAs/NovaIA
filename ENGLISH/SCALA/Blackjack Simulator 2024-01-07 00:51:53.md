```scala
// Define a case class to represent a playing card
case class Card(rank: String, suit: String)

// Define a sealed trait to represent the different suits of a playing card
sealed trait Suit
object Spades extends Suit
object Hearts extends Suit
object Diamonds extends Suit
object Clubs extends Suit

// Define a sealed trait to represent the different ranks of a playing card
sealed trait Rank
object Ace extends Rank
object Two extends Rank
object Three extends Rank
object Four extends Rank
object Five extends Rank
object Six extends Rank
object Seven extends Rank
object Eight extends Rank
object Nine extends Rank
object Ten extends Rank
object Jack extends Rank
object Queen extends Rank
object King extends Rank

// Define a function to generate a standard deck of cards
def generateDeck(): List[Card] = {
  for {
    suit <- List(Spades, Hearts, Diamonds, Clubs)
    rank <- List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
  } yield Card(rank.toString, suit.toString)
}

// Define a function to shuffle a deck of cards
def shuffleDeck(deck: List[Card]): List[Card] = {
  scala.util.Random.shuffle(deck)
}

// Define a function to deal a hand of cards to a player
def dealHand(deck: List[Card], numCards: Int): List[Card] = {
  deck.take(numCards)
}

// Define a function to calculate the score of a hand of cards
def calculateScore(hand: List[Card]): Int = {
  hand.foldLeft(0) { (score, card) =>
    card.rank match {
      case Ace => score + 11
      case Two => score + 2
      case Three => score + 3
      case Four => score + 4
      case Five => score + 5
      case Six => score + 6
      case Seven => score + 7
      case Eight => score + 8
      case Nine => score + 9
      case Ten | Jack | Queen | King => score + 10
    }
  }
}

// Define a function to play a game of blackjack
def playBlackjack(deck: List[Card]): Unit = {
  // Deal two cards to the player
  val playerHand = dealHand(deck, 2)

  // Deal two cards to the dealer
  val dealerHand = dealHand(deck, 2)

  // Calculate the scores of the player and dealer hands
  val playerScore = calculateScore(playerHand)
  val dealerScore = calculateScore(dealerHand)

  // Print the player's hand and score
  println("Your hand:")
  playerHand.foreach(println)
  println(s"Your score: $playerScore")

  // Print the dealer's hand and score
  println("Dealer's hand:")
  dealerHand.foreach(println)
  println(s"Dealer's score: $dealerScore")

  // Check if the player or dealer has blackjack
  if (playerScore == 21 && dealerScore == 21) {
    println("Push!")
  } else if (playerScore == 21) {
    println("Blackjack! You win!")
  } else if (dealerScore == 21) {
    println("Blackjack! Dealer wins!")
  } else {
    // Allow the player to hit or stand
    while (playerScore < 21) {
      println("Hit or stand? (h/s)")
      val input = scala.io.StdIn.readLine()

      if (input == "h") {
        // Deal a card to the player
        val card = dealHand(deck, 1).head
        playerHand += card

        // Calculate the player's score
        playerScore = calculateScore(playerHand)

        // Print the player's hand and score
        println("Your hand:")
        playerHand.foreach(println)
        println(s"Your score: $playerScore")
      } else if (input == "s") {
        // Break out of the loop
        break
      } else {
        println("Invalid input. Please enter 'h' to hit or 's' to stand.")
      }
    }

    // Allow the dealer to hit until they reach 17 or bust
    while (dealerScore < 17) {
      // Deal a card to the dealer
      val card = dealHand(deck, 1).head
      dealerHand += card

      // Calculate the dealer's score
      dealerScore = calculateScore(dealerHand)

      // Print the dealer's hand and score
      println("Dealer's hand:")
      dealerHand.foreach(println)
      println(s"Dealer's score: $dealerScore")
    }

    // Determine the winner
    if (playerScore > 21 && dealerScore > 21) {
      println("Both players bust! Dealer wins!")
    } else if (playerScore > 21) {
      println("You bust! Dealer wins!")
    } else if (dealerScore > 21) {
      println("Dealer busts! You win!")
    } else if (playerScore == dealerScore) {
      println("Push!")
    } else if (playerScore > dealerScore) {
      println("You win!")
    } else {
      println("Dealer wins!")
    }
  }
}

// Generate a new deck of cards
val deck = generateDeck()

// Shuffle the deck
val shuffledDeck = shuffleDeck(deck)

// Play a game of blackjack
playBlackjack(shuffledDeck)
```

This code simulates a game of blackjack in Scala. It first defines a case class to represent a playing card, a sealed trait to represent the different suits of a card, and a sealed trait to represent the different ranks of a card. It then defines functions to generate a standard deck of cards, shuffle the deck, deal a hand of cards to a player, calculate the score of a hand of cards, and play a game of blackjack. The `playBlackjack` function first deals two cards to the player and dealer, calculates their scores, and prints them to the console. It then allows the player to hit or stand, and the dealer to hit until they reach 17 or bust. Finally, it determines the winner and prints the result to the console.