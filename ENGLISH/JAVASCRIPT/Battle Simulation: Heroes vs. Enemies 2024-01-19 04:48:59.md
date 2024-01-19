```javascript
// Enemies array contains objects that represent each enemy's properties
const enemies = [
  { id: 1, name: 'Enemy 1', health: 100, attack: 20, defense: 10 },
  { id: 2, name: 'Enemy 2', health: 80, attack: 30, defense: 15 },
  { id: 3, name: 'Enemy 3', health: 120, attack: 25, defense: 20 }
];

// Heroes array contains objects that represent each hero's properties
const heroes = [
  { id: 1, name: 'Hero 1', health: 100, attack: 25, defense: 15 },
  { id: 2, name: 'Hero 2', health: 80, attack: 35, defense: 10 },
  { id: 3, name: 'Hero 3', health: 120, attack: 20, defense: 25 }
];

// Function to calculate damage dealt by one entity (attacker) to another (defender)
function calculateDamage(attacker, defender) {
  // Damage is based on attacker's attack power minus defender's defense
  const damage = attacker.attack - defender.defense;

  // If damage is negative or zero, set it to 1 to prevent healing
  return Math.max(1, damage);
}

// Function to simulate a battle between two entities
function battle(attacker, defender) {
  // While both entities are alive, take turns dealing damage to each other
  while (attacker.health > 0 && defender.health > 0) {
    // Calculate damage dealt by each entity to the other
    const attackerDamage = calculateDamage(attacker, defender);
    const defenderDamage = calculateDamage(defender, attacker);

    // Subtract damage from each entity's health
    attacker.health -= defenderDamage;
    defender.health -= attackerDamage;

    // Log the current health of each entity
    console.log(`${attacker.name}: ${attacker.health}, ${defender.name}: ${defender.health}`);
  }

  // Determine the winner based on which entity has more health remaining
  const winner = attacker.health > defender.health ? attacker : defender;

  // Log the winner of the battle
  console.log(`The winner is ${winner.name}!`);
}

// Main function to simulate battles between all enemies and heroes
function main() {
  // Iterate over each enemy and each hero
  for (let i = 0; i < enemies.length; i++) {
    for (let j = 0; j < heroes.length; j++) {
      // Simulate a battle between the current enemy and the current hero
      battle(enemies[i], heroes[j]);

      // Separate each battle with a line break for clarity
      console.log('---------------------------');
    }
  }
}

// Call the main function to start the battle simulations
main();
```

Explanation:

1. We have two arrays, `enemies` and `heroes`, each containing objects representing the properties of each enemy and hero respectively (ID, name, health, attack, and defense).

2. The `calculateDamage` function calculates the damage dealt by one entity (`attacker`) to another (`defender`). It subtracts the defender's defense from the attacker's attack, but ensures the damage is never negative or zero.

3. The `battle` function simulates a battle between two entities. It alternates between the two entities, dealing damage to each other until one of them has no health remaining. It logs the health of both entities as the battle progresses and declares the winner at the end.

4. The `main` function iterates over all enemies and heroes, simulating battles between each enemy and each hero. It separates each battle with a line break for clarity.

5. Finally, we call the `main` function to start the battle simulations.

This code simulates multiple battles between enemies and heroes, displaying the results and declaring the winners.