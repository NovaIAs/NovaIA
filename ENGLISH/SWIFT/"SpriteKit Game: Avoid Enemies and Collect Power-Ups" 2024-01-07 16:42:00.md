```swift
// Import the necessary libraries
import UIKit
import SpriteKit
import AVFoundation

// Define the main game class
class MyGame: SKScene {

    // Initialize the game scene
    override func didMove(to view: SKView) {
        // Set the background color
        self.backgroundColor = UIColor.black

        // Create the player character
        let player = SKSpriteNode(imageNamed: "player")
        player.position = CGPoint(x: self.frame.width / 2, y: self.frame.height / 2)
        self.addChild(player)

        // Create the enemy characters
        for i in 0...10 {
            let enemy = SKSpriteNode(imageNamed: "enemy")
            enemy.position = CGPoint(x: CGFloat.random(in: 0...self.frame.width), y: CGFloat.random(in: 0...self.frame.height))
            self.addChild(enemy)
        }

        // Create the power-up characters
        for i in 0...5 {
            let powerUp = SKSpriteNode(imageNamed: "powerUp")
            powerUp.position = CGPoint(x: CGFloat.random(in: 0...self.frame.width), y: CGFloat.random(in: 0...self.frame.height))
            self.addChild(powerUp)
        }

        // Create the background music
        let backgroundMusic = SKAudioNode(fileNamed: "backgroundMusic.mp3")
        backgroundMusic.autoplayLooped = true
        self.addChild(backgroundMusic)

        // Start the game loop
        self.run(SKAction.repeatForever(SKAction.sequence([
            SKAction.wait(forDuration: 1),
            SKAction.run {
                // Update the game state
                self.update()
            }
        ])))
    }

    // Update the game state
    func update() {
        // Move the player character
        if self.player.position.x < 0 || self.player.position.x > self.frame.width {
            self.player.position.x = self.frame.width / 2
        }
        if self.player.position.y < 0 || self.player.position.y > self.frame.height {
            self.player.position.y = self.frame.height / 2
        }

        // Move the enemy characters
        for enemy in self.children where enemy is SKSpriteNode && enemy.name == "enemy" {
            enemy.position = CGPoint(x: enemy.position.x + CGFloat.random(in: -5...5), y: enemy.position.y + CGFloat.random(in: -5...5))
        }

        // Move the power-up characters
        for powerUp in self.children where powerUp is SKSpriteNode && powerUp.name == "powerUp" {
            powerUp.position = CGPoint(x: powerUp.position.x + CGFloat.random(in: -5...5), y: powerUp.position.y + CGFloat.random(in: -5...5))
        }

        // Check for collisions between the player and the enemy characters
        for enemy in self.children where enemy is SKSpriteNode && enemy.name == "enemy" {
            if self.player.intersects(enemy) {
                // The player has collided with an enemy character
                self.gameOver()
            }
        }

        // Check for collisions between the player and the power-up characters
        for powerUp in self.children where powerUp is SKSpriteNode && powerUp.name == "powerUp" {
            if self.player.intersects(powerUp) {
                // The player has collided with a power-up character
                self.powerUp()
            }
        }
    }

    // Game over
    func gameOver() {
        // Stop the game loop
        self.removeAllActions()

        // Display the game over message
        let gameOverLabel = SKLabelNode(text: "Game Over")
        gameOverLabel.position = CGPoint(x: self.frame.width / 2, y: self.frame.height / 2)
        self.addChild(gameOverLabel)
    }

    // Power up
    func powerUp() {
        // Increase the player's speed
        self.player.speed *= 2

        // Increase the player's health
        self.player.health += 1

        // Display the power up message
        let powerUpLabel = SKLabelNode(text: "Power Up!")
        powerUpLabel.position = CGPoint(x: self.frame.width / 2, y: self.frame.height / 2)
        self.addChild(powerUpLabel)
    }
}

// Create the game view
let gameView = SKView(frame: CGRect(x: 0, y: 0, width: 640, height: 480))

// Create the game scene
let gameScene = MyGame(size: gameView.frame.size)

// Add the game scene to the game view
gameView.presentScene(gameScene)

// Start the game
gameView.ignoresSiblingOrder = true
gameView.showsFPS = true
gameView.showsNodeCount = true
```

This code creates a simple game in which the player controls a character that moves around the screen and collects power-ups while avoiding enemy characters. The game is written in Swift and uses the SpriteKit framework to create the game scene and characters. The code includes a game loop that updates the game state and checks for collisions between the player and the enemy and power-up characters. The code also includes a game over function that is called when the player collides with an enemy character, and a power up function that is called when the player collides with a power-up character.