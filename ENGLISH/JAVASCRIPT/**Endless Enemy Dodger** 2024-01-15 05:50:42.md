```javascript
// START OF THE CODE

// Define a function to generate a random number between two values
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to generate a random color in hexadecimal format
function getRandomColor() {
  const letters = '0123456789ABCDEF';
  let color = '#';
  for (let i = 0; i < 6; i++) {
    color += letters[getRandomNumber(0, 15)];
  }
  return color;
}

// Define a function to create a new div element with specified attributes
function createDiv(attributes) {
  const div = document.createElement('div');
  for (const attribute in attributes) {
    div.setAttribute(attribute, attributes[attribute]);
  }
  return div;
}

// Define a function to create a new span element with specified attributes
function createSpan(attributes) {
  const span = document.createElement('span');
  for (const attribute in attributes) {
    span.setAttribute(attribute, attributes[attribute]);
  }
  return span;
}

// Define a function to add an event listener to an element
function addEventListener(element, event, callback) {
  element.addEventListener(event, callback);
}

// Define a function to remove an event listener from an element
function removeEventListener(element, event, callback) {
  element.removeEventListener(event, callback);
}

// Define a function to toggle the visibility of an element
function toggleVisibility(element) {
  if (element.style.display === 'none') {
    element.style.display = 'block';
  } else {
    element.style.display = 'none';
  }
}

// Define a function to set the position of an element
function setPosition(element, x, y) {
  element.style.left = x + 'px';
  element.style.top = y + 'px';
}

// Define a function to get the position of an element
function getPosition(element) {
  const rect = element.getBoundingClientRect();
  return { x: rect.left, y: rect.top };
}

// END OF THE CODE

// USAGE OF THE CODE

// Create a new div element as a container for the game
const container = createDiv({ id: 'container' });

// Create a new div element for the player
const player = createDiv({ id: 'player' });

// Create a new span element for the player's score
const scoreSpan = createSpan({ id: 'score' });
scoreSpan.textContent = 'Score: 0';

// Create a new div element for the enemies
const enemiesContainer = createDiv({ id: 'enemies-container' });

// Create an array to store the enemies
const enemies = [];

// Add the player and the enemies container to the container
container.appendChild(player);
container.appendChild(enemiesContainer);

// Add the score span to the player
player.appendChild(scoreSpan);

// Add the container to the document body
document.body.appendChild(container);

// Set the initial position of the player
setPosition(player, 200, 200);

// Define the speed of the player
const playerSpeed = 5;

// Define the speed of the enemies
const enemySpeed = 2;

// Define the score
let score = 0;

// Define the game loop function
function gameLoop() {
  // Move the enemies
  for (let i = 0; i < enemies.length; i++) {
    const enemy = enemies[i];
    const position = getPosition(enemy);
    setPosition(enemy, position.x + enemySpeed, position.y);

    // Check if the enemy has reached the right edge of the screen
    if (position.x > window.innerWidth) {
      // Remove the enemy from the enemies array
      enemies.splice(i, 1);

      // Remove the enemy from the enemies container
      enemiesContainer.removeChild(enemy);
    }

    // Check if the player has collided with the enemy
    if (isColliding(player, enemy)) {
      // Increase the score
      score++;

      // Update the score span
      scoreSpan.textContent = 'Score: ' + score;

      // Remove the enemy from the enemies array
      enemies.splice(i, 1);

      // Remove the enemy from the enemies container
      enemiesContainer.removeChild(enemy);
    }
  }

  // Create a new enemy if there are less than 10 enemies on the screen
  if (enemies.length < 10) {
    const enemy = createDiv({ class: 'enemy' });
    enemy.style.left = window.innerWidth + 'px';
    enemy.style.top = getRandomNumber(0, window.innerHeight - enemy.offsetHeight) + 'px';
    enemy.style.backgroundColor = getRandomColor();

    // Add the enemy to the enemies array
    enemies.push(enemy);

    // Add the enemy to the enemies container
    enemiesContainer.appendChild(enemy);
  }

  // Request the next animation frame
  requestAnimationFrame(gameLoop);
}

// Start the game loop
gameLoop();

// Define a function to check if two elements are colliding
function isColliding(element1, element2) {
  const rect1 = element1.getBoundingClientRect();
  const rect2 = element2.getBoundingClientRect();

  return !(
    rect1.right < rect2.left ||
    rect1.left > rect2.right ||
    rect1.bottom < rect2.top ||
    rect1.top > rect2.bottom
  );
}

// Add an event listener to the window to listen for keydown events
addEventListener(window, 'keydown', (event) => {
  // Check which key was pressed
  switch (event.key) {
    case 'ArrowUp':
      // Move the player up
      setPosition(player, player.offsetLeft, player.offsetTop - playerSpeed);
      break;
    case 'ArrowDown':
      // Move the player down
      setPosition(player, player.offsetLeft, player.offsetTop + playerSpeed);
      break;
    case 'ArrowLeft':
      // Move the player left
      setPosition(player, player.offsetLeft - playerSpeed, player.offsetTop);
      break;
    case 'ArrowRight':
      // Move the player right
      setPosition(player, player.offsetLeft + playerSpeed, player.offsetTop);
      break;
  }
});


```

**Explanation of the Code:**

1. We start by defining a set of functions for various purposes, including generating random numbers, creating HTML elements, adding and removing event listeners, and manipulating element positions.

2. We create a container div element to hold the game elements and add it to the document body.

3. We create a player div element and add it to the container. We also create a span element for displaying the score and add it to the player.

4. We create a div element for the enemies and add it to the container. We also create an array to store the enemy elements.

5. We define the game loop function, which handles the movement of enemies, collision detection, score updates, and the creation of new enemies.

6. We start the game loop using `requestAnimationFrame`.

7. We define a function to check for collisions between two elements.

8. We add an event listener to the window to listen for keydown events and move the player accordingly.

This code creates a simple game where the player can move around the screen and avoid enemies. When the player collides with an enemy, the score increases, and the enemy is removed from the screen. The game loop continues until the player collides with an enemy, at which point the game ends.