```typescript
// 1. Class with Generic Interface
interface User<T> {
  name: string;
  data: T;
}

class GenericUser<T> implements User<T> {
  name: string;
  data: T;

  constructor(name: string, data: T) {
    this.name = name;
    this.data = data;
  }

  displayData(): void {
    console.log(`User: ${this.name}, Data: ${this.data}`);
  }
}

// 2. Object Destructuring
const userDetails = {
  name: 'John Doe',
  age: 30,
  hobbies: ['coding', 'reading', 'hiking'],
};

const { name, age, hobbies } = userDetails;

console.log(`Name: ${name}, Age: ${age}, Hobbies: ${hobbies}`);

// 3. Advanced Types: Union Types and Type Aliases
type PrimitiveType = string | number | boolean;

type ComplexType = {
  name: string;
  age: number;
};

const primitive: PrimitiveType = 'Hello World';
const complex: ComplexType = { name: 'John Doe', age: 30 };

// 4. Advanced Function Types: Rest Parameters and Optional Parameters
function sumNumbers(...nums: number[]): number {
  let total = 0;
  for (const num of nums) {
    total += num;
  }
  return total;
}

function greet(name: string, greeting?: string): string {
  if (greeting) {
    return `Hello ${name}, ${greeting}!`;
  } else {
    return `Hello ${name}!`;
  }
}

// 5. Modules: Import and Export
// file1.ts
export const PI = 3.14;

export function calculateArea(radius: number): number {
  return PI * radius ** 2;
}

// file2.ts
import { PI, calculateArea } from './file1';

const radius = 5;
const area = calculateArea(radius);

console.log(`Area of a circle with radius ${radius} is: ${area}`);

// 6. Classes: Inheritance and Polymorphism
class Animal {
  name: string;
  constructor(name: string) {
    this.name = name;
  }

  speak(): void {
    console.log('I am an animal.');
  }
}

class Dog extends Animal {
  constructor(name: string) {
    super(name);
  }

  bark(): void {
    console.log('Woof!');
  }
}

class Cat extends Animal {
  constructor(name: string) {
    super(name);
  }

  meow(): void {
    console.log('Meow!');
  }
}

const dog = new Dog('Rex');
dog.speak(); // I am an animal.
dog.bark(); // Woof!

const cat = new Cat('Kitty');
cat.speak(); // I am an animal.
cat.meow(); // Meow!

// 7. Asynchronous Programming: Promises and Callbacks
const promise = new Promise((resolve, reject) => {
  setTimeout(() => {
    resolve('Hello from the promise!');
  }, 2000);
});

promise.then((data) => {
  console.log(data); // Hello from the promise!
});

// Callback function
const callbackFunction = (data: string) => {
  console.log(data); // Hello from the callback!
};

setTimeout(callbackFunction, 2000, 'Hello from the callback!');

// 8. Error Handling: try...catch...finally
try {
  // Code that may throw an error
  throw new Error('Oops, something went wrong!');
} catch (error) {
  // Handle the error
  console.log(`Error: ${error.message}`);
} finally {
  // Code that always runs, regardless of whether an error was thrown or not
  console.log('This will always run.');
}

// 9. Regular Expressions
const pattern = /ab+c/; // Matches "abc", "abbc", "abbbc", etc.
const input = 'abbbc';

if (pattern.test(input)) {
  console.log('The input matches the pattern.');
} else {
  console.log('The input does not match the pattern.');
}

// 10. Advanced Data Structures: Maps and Sets
const map = new Map();
map.set('key1', 'value1');
map.set('key2', 'value2');

map.forEach((value, key) => {
  console.log(`Key: ${key}, Value: ${value}`);
});

const set = new Set();
set.add('item1');
set.add('item2');
set.add('item3');

set.forEach((item) => {
  console.log(`Item: ${item}`);
});
```

**Explanation:**

This is a comprehensive TypeScript code that covers various advanced concepts and techniques in the language. Let's break down each section:

**1. Class with Generic Interface:**
   - Defines a generic interface `User` that can hold data of any type.
   - Creates a generic class `GenericUser` that implements the interface.

**2. Object Destructuring:**
   - Destructures an object into individual variables using the object destructuring syntax.

**3. Advanced Types:**
   - Demonstrates union types and type aliases to define complex and reusable types.

**4. Advanced Function Types:**
   - Utilizes rest parameters and optional parameters in function definitions.

**5. Modules: Import and Export:**
   - Shows how to export and import modules to organize code into reusable units.

**6. Classes: Inheritance and Polymorphism:**
   - Defines parent and child classes with inheritance and method overriding.

**7. Asynchronous Programming:**
   - Demonstrates the use of promises and callbacks for asynchronous operations.

**8. Error Handling:**
   - Uses `try...catch...finally` for error handling and graceful cleanup.

**9. Regular Expressions:**
   - Utilizes regular expressions for pattern matching and validation.

**10. Advanced Data Structures:**
    - Introduces maps and sets for efficient key-value storage and unique item collections.

This code showcases various advanced features and techniques in TypeScript, allowing you to write more complex and efficient code.