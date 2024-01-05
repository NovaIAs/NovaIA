```haskell
-- Import the necessary libraries
import Data.List (nub, sort)
import Data.Ord (comparing)
import System.Random (randomRIO)

-- Define the type of a deck of cards
type Deck = [Card]

-- Define the type of a card
data Card = 
    Ace 
  | Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Ten 
  | Jack 
  | Queen 
  | King 
  deriving (Eq, Ord, Show)

-- Define the type of a suit
data Suit = 
    Hearts 
  | Diamonds 
  | Spades 
  | Clubs 
  deriving (Eq, Ord, Show)

-- Create a new deck of cards
newDeck :: Deck
newDeck = [c | s <- [Hearts, Diamonds, Spades, Clubs], c <- [Ace .. King]]

-- Shuffle a deck of cards
shuffle :: Deck -> IO Deck
shuffle deck = do
  n <- randomRIO (0, length deck - 1)
  let (a, b) = splitAt n deck
  return $ b ++ a

-- Deal a hand of cards
dealHand :: Int -> Deck -> IO [Card]
dealHand n deck = do
  hand <- take n <$> shuffle deck
  return $ nub hand

-- Evaluate a hand of cards
evaluateHand :: [Card] -> Int
evaluateHand hand = sum $ map cardValue hand

-- Get the card value
cardValue :: Card -> Int
cardValue Ace = 1
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10

-- Get the highest card in a hand
highestCard :: [Card] -> Card
highestCard hand = maximum hand

-- Get the lowest card in a hand
lowestCard :: [Card] -> Card
lowestCard hand = minimum hand

-- Check if a hand contains an ace
hasAce :: [Card] -> Bool
hasAce hand = any (== Ace) hand

-- Check if a hand contains a pair
hasPair :: [Card] -> Bool
hasPair hand = length (filter (== head hand) (tail hand)) > 0

-- Check if a hand contains two pairs
hasTwoPairs :: [Card] -> Bool
hasTwoPairs hand = length (filter (== head hand) (tail hand)) > 0 && length (filter (== head (tail hand)) (tail (tail hand))) > 0

-- Check if a hand contains three of a kind
hasThreeOfAKind :: [Card] -> Bool
hasThreeOfAKind hand = length (filter (== head hand) (tail hand)) > 1

-- Check if a hand contains a straight
hasStraight :: [Card] -> Bool
hasStraight hand = and $ zipWith (== 1) (sort hand) (tail (sort hand))

-- Check if a hand contains a flush
hasFlush :: [Card] -> Bool
hasFlush hand = all (== head hand) (map cardSuit hand)

-- Check if a hand contains a full house
hasFullHouse :: [Card] -> Bool
hasFullHouse hand = hasThreeOfAKind hand && hasPair hand

-- Check if a hand contains a four of a kind
hasFourOfAKind :: [Card] -> Bool
hasFourOfAKind hand = length (filter (== head hand) (tail hand)) > 2

-- Check if a hand contains a straight flush
hasStraightFlush :: [Card] -> Bool
hasStraightFlush hand = hasStraight hand && hasFlush hand

-- Check if a hand contains a royal flush
hasRoyalFlush :: [Card] -> Bool
hasRoyalFlush hand = hasStraightFlush hand && highestCard hand == Ace

-- Print the hand
printHand :: [Card] -> IO ()
printHand hand = mapM_ print hand

-- Print the evaluation of the hand
printEvaluation :: [Card] -> IO ()
printEvaluation hand = print $ evaluateHand hand

-- Main function
main :: IO ()
main = do
  deck <- newDeck
  hand <- dealHand 5 deck
  printHand hand
  printEvaluation hand
```

This code is a Haskell implementation of a poker hand evaluator. It defines the types of a deck of cards, a card, and a suit. It also defines functions to create a new deck of cards, shuffle a deck of cards, deal a hand of cards, evaluate a hand of cards, and check if a hand contains various poker hands, such as a pair, two pairs, three of a kind, a straight, a flush, a full house, four of a kind, a straight flush, and a royal flush. The main function creates a new deck of cards, deals a hand of five cards, and prints the hand and its evaluation.