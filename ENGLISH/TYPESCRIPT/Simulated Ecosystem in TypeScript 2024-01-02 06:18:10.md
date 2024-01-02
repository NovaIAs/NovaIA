Certainly! Here's a complex TypeScript code that demonstrates a simulated ecosystem with various organisms interacting with each other:

```typescript
interface Organism {
  name: string;
  type: string;
  health: number;
  position: { x: number; y: number };
  move(): void;
  reproduce(): Organism;
  interact(organism: Organism): void;
}

class Animal implements Organism {
  name: string;
  type: string;
  health: number;
  position: { x: number; y: number };

  constructor(name: string, type: string, position: { x: number; y: number }) {
    this.name = name;
    this.type = type;
    this.health = 100;
    this.position = position;
  }

  move(): void {
    const newX = Math.random() * 10;
    const newY = Math.random() * 10;
    this.position = { x: this.position.x + newX, y: this.position.y + newY };
    console.log(`${this.name} moved to (${this.position.x}, ${this.position.y})`);
  }

  reproduce(): Animal {
    const babyName = `${this.name} Jr.`;
    const babyType = this.type;
    const babyPosition = { x: this.position.x, y: this.position.y };
    return new Animal(babyName, babyType, babyPosition);
  }

  interact(organism: Organism): void {
    if (organism instanceof Animal) {
      console.log(`${this.name} is interacting with ${organism.name}`);
      this.health -= 10;
      organism.health -= 10;
    } else {
      console.log(`${this.name} can only interact with other animals`);
    }
  }
}

class Plant implements Organism {
  name: string;
  type: string;
  health: number;
  position: { x: number; y: number };

  constructor(name: string, type: string, position: { x: number; y: number }) {
    this.name = name;
    this.type = type;
    this.health = 100;
    this.position = position;
  }

  move(): void {
    console.log(`${this.name} cannot move.`);
  }

  reproduce(): Plant {
    const babyName = `${this.name} offspring`;
    const babyType = this.type;
    const babyPosition = { x: this.position.x, y: this.position.y };
    return new Plant(babyName, babyType, babyPosition);
  }

  interact(organism: Organism): void {
    console.log(`${this.name} is interacting with ${organism.name}`);
    this.health -= 5;
  }
}

const lion = new Animal("Lion", "Predator", { x: 0, y: 0 });
const zebra = new Animal("Zebra", "Prey", { x: 5, y: 5 });
const tree = new Plant("Tree", "Flora", { x: 10, y: 10 });

lion.move(); // Lion moved to (8.714342, 6.204437)
zebra.move(); // Zebra moved to (12.252544, 12.766685)
tree.move(); // Tree cannot move.

lion.interact(zebra); // Lion is interacting with Zebra
console.log(`Lion's Health: ${lion.health}`); // Lion's Health: 90
console.log(`Zebra's Health: ${zebra.health}`); // Zebra's Health: 90

tree.interact(lion); // Tree is interacting with Lion
console.log(`Tree's Health: ${tree.health}`); // Tree's Health: 95

const lionCub = lion.reproduce();
console.log(`Lion Cub: ${lionCub.name}, ${lionCub.type}`); // Lion Cub: Lion Jr., Predator
```

Explanation:
- The code starts by defining an `Organism` interface, which has common properties and methods shared by both animals and plants.
- Then, two classes `Animal` and `Plant` are implemented, both implementing the `Organism` interface.
- Animals have additional methods like `move()` to simulate movement, `reproduce()` to create offspring, and `interact()` to interact with other organisms.
- Plants, being immobile, have a different `move()` behavior, and their `interact()` method affects only their own health.
- The code creates instances of animals (lion and zebra) and a plant (tree).
- The organisms are then subjected to various interactions and movements, showcasing the simulated ecosystem behavior.
- The code concludes by demonstrating the reproduction of a lion and displaying the properties of the resulting lion cub.

Note: This code is just an example to showcase interactions in an ecosystem and can be further expanded and customized to accommodate more complex scenarios.