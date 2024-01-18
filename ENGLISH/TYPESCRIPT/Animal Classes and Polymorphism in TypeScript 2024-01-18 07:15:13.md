```typescript
interface Animal {
  name: string;
  age: number;
  type: string;
  makeSound(): string;
}

class Dog implements Animal {
  constructor(public name: string, public age: number) {
    this.type = 'dog';
  }
  makeSound(): string {
    return 'Woof!';
  }
}

class Cat implements Animal {
  constructor(public name: string, public age: number) {
    this.type = 'cat';
  }
  makeSound(): string {
    return 'Meow!';
  }
}

class Bird implements Animal {
  constructor(public name: string, public age: number) {
    this.type = 'bird';
  }
  makeSound(): string {
    return 'Chirp!';
  }
}

function printAnimalInfo(animal: Animal) {
  console.log(`Animal: ${animal.name}`);
  console.log(`Age: ${animal.age}`);
  console.log(`Type: ${animal.type}`);
  console.log(`Sound: ${animal.makeSound()}`);
}

const dog = new Dog('Buddy', 5);
const cat = new Cat('Kitty', 3);
const bird = new Bird('Tweety', 1);

printAnimalInfo(dog);
printAnimalInfo(cat);
printAnimalInfo(bird);
```

Explanation:

1. We define an interface `Animal` that represents the common properties and methods of different animals. It has properties for `name`, `age`, and `type`, as well as a `makeSound()` method.

2. We define three classes `Dog`, `Cat`, and `Bird` that implement the `Animal` interface. Each class has its own implementation of the `makeSound()` method to represent the different sounds made by each animal.

3. We define a function `printAnimalInfo()` that takes an `Animal` object and prints its information, including its name, age, type, and the sound it makes.

4. We create instances of the `Dog`, `Cat`, and `Bird` classes and store them in variables named `dog`, `cat`, and `bird`, respectively.

5. We call the `printAnimalInfo()` function with each animal object to print their information.

When you run this code, it will print the following output:

```
Animal: Buddy
Age: 5
Type: dog
Sound: Woof!

Animal: Kitty
Age: 3
Type: cat
Sound: Meow!

Animal: Tweety
Age: 1
Type: bird
Sound: Chirp!
```

This code demonstrates the use of interfaces, classes, inheritance, and polymorphism in TypeScript to model different types of animals and their behaviors.