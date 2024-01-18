```rust
// This is a Rust program that simulates a simple game of blackjack.

// First, we need to define the deck of cards.
const DECK: [char; 52] = [
    '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
    '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
    '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
    '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
];

// Next, we need to define the players.
struct Player {
    name: String,
    hand: Vec<char>,
    score: i32,
}

// Now, we can start the game.
fn main() {
    // First, we need to create the players.
    let player1 = Player {
        name: "Player 1",
        hand: Vec::new(),
        score: 0,
    };

    let player2 = Player {
        name: "Player 2",
        hand: Vec::new(),
        score: 0,
    };

    // Now, we need to deal the cards.
    for _ in 0..2 {
        player1.hand.push(DECK[rand::random::<usize>() % 52]);
        player2.hand.push(DECK[rand::random::<usize>() % 52]);
    }

    // Now, we can start the game loop.
    loop {
        // First, we need to check if either player has blackjack.
        if player1.score == 21 || player2.score == 21 {
            // If so, the player with blackjack wins.
            if player1.score == 21 {
                println!("Player 1 wins!");
            } else {
                println!("Player 2 wins!");
            }

            break;
        }

        // If neither player has blackjack, we need to check if either player has busted.
        if player1.score > 21 || player2.score > 21 {
            // If so, the player who busted loses.
            if player1.score > 21 {
                println!("Player 1 busts!");
                println!("Player 2 wins!");
            } else {
                println!("Player 2 busts!");
                println!("Player 1 wins!");
            }

            break;
        }

        // If neither player has blackjack or busted, we need to check if either player wants to hit or stand.
        let player1_input = get_input(&player1.name);
        let player2_input = get_input(&player2.name);

        // If both players stand, the game ends.
        if player1_input == "stand" && player2_input == "stand" {
            // The player with the highest score wins.
            if player1.score > player2.score {
                println!("Player 1 wins!");
            } else if player2.score > player1.score {
                println!("Player 2 wins!");
            } else {
                println!("Tie!");
            }

            break;
        }

        // If only one player stands, the other player can continue to hit.
        if player1_input == "stand" {
            player2.hand.push(DECK[rand::random::<usize>() % 52]);
            player2.score += get_card_value(player2.hand.last().unwrap());
        } else if player2_input == "stand" {
            player1.hand.push(DECK[rand::random::<usize>() % 52]);
            player1.score += get_card_value(player1.hand.last().unwrap());
        }

        // If both players hit, we need to deal them a new card.
        player1.hand.push(DECK[rand::random::<usize>() % 52]);
        player2.hand.push(DECK[rand::random::<usize>() % 52]);

        // We also need to update their scores.
        player1.score += get_card_value(player1.hand.last().unwrap());
        player2.score += get_card_value(player2.hand.