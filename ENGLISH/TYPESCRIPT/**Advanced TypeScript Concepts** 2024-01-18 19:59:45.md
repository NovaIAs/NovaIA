**1. Type Aliases:**

```typescript
type Person = {
  name: string;
  age: number;
  occupation: string;
  hobbies: string[];
};

type Address = {
  street: string;
  city: string;
  state: string;
  zipCode: number;
};
```

**Explanation:**
We define two type aliases, `Person` and `Address`, to represent the data structures for a person and an address, respectively. This makes the code more concise and easier to understand.

**2. Intersection Types:**

```typescript
type PersonWithAddress = Person & Address;

const personWithAddress: PersonWithAddress = {
  name: "John Smith",
  age: 30,
  occupation: "Software Engineer",
  hobbies: ["Coding", "Hiking", "Photography"],
  street: "123 Main Street",
  city: "Anytown",
  state: "CA",
  zipCode: 91234,
};
```

**Explanation:**
We use the intersection type operator (`&`) to combine the `Person` and `Address` types into a new type called `PersonWithAddress`. This allows us to represent a person who also has an address.

**3. Union Types:**

```typescript
type Pet = {
  type: "dog" | "cat";
  name: string;
  age: number;
};

const pets: Pet[] = [
  { type: "dog", name: "Buddy", age: 5 },
  { type: "cat", name: "Whiskers", age: 3 },
];
```

**Explanation:**
We define a `Pet` type that can be either a dog or a cat using the union type operator (`|`). This allows us to represent a list of pets that can contain both dogs and cats.

**4. Conditional Types:**

```typescript
type IsEven<N extends number> = (N extends 0 ? true : N extends 1 ? false : IsEven<N - 2>);

const isEven = IsEven<10>; // true
const isOdd = IsEven<9>; // false
```

**Explanation:**
We define a conditional type called `IsEven` that takes a numeric type parameter `N` and returns a boolean indicating whether `N` is even or not. This is a recursive type that uses the `extends` keyword to check different cases.

**5. Template Literal Types:**

```typescript
type Status = "active" | "inactive" | "pending";

type Action<S extends Status> = `make-${S}`;

const makeActive: Action<"inactive"> = "make-inactive";
const makeInactive: Action<"active"> = "make-active";
```

**Explanation:**
We use template literal types to define a type called `Action` that takes a status type parameter `S` and generates a string literal that represents an action to be performed on that status. This allows us to create type-safe action strings based on the status.

**6. Mapped Types:**

```typescript
type MappedPerson = {
  [key in keyof Person]: Person[key];
};

const mappedPerson: MappedPerson = {
  name: "Jane Doe",
  age: 25,
  occupation: "Teacher",
  hobbies: ["Reading", "Cooking", "Traveling"],
};
```

**Explanation:**
We use a mapped type to create a new type called `MappedPerson` that has the same properties as the `Person` type, but the property names and types are dynamically generated using the `keyof` and `Person[key]` constructs. This allows us to create a new object with the same structure as a `Person` object.

**7. Higher-Order Functions:**

```typescript
function map<T, U>(array: T[], fn: (item: T) => U): U[] {
  const result: U[] = [];
  for (const item of array) {
    result.push(fn(item));
  }
  return result;
}

const numbers = [1, 2, 3, 4, 5];
const doubledNumbers = map(numbers, (n) => n * 2); // [2, 4, 6, 8, 10]
```

**Explanation:**
We define a higher-order function called `map` that takes an array and a callback function as arguments and returns a new array containing the results of applying the callback function to each element of the array. We use this function to double the values in an array of numbers.

**8. Decorators:**

```typescript
function Logger(target: Function) {
  const original = target.prototype.log;
  target.prototype.log = function (...args: any[]) {
    console.log("LOGGER START");
    original.apply(this, args);
    console.log("LOGGER END");
  };
}

@Logger
class MyClass {
  log(...args: any[]) {
    console.log(...args);
  }
}

const instance = new MyClass();
instance.log("Hello, world!"); // Output: LOGGER START, Hello, world!, LOGGER END
```

**Explanation:**
We define a decorator called `Logger` that takes a class as an argument and wraps its `log` method to add logging functionality. We use the `@` symbol to apply the decorator to the `MyClass` class. When we call the `log` method on an instance of `MyClass`, the decorator's wrapper function is executed, which logs messages before and after the original `log` method is called.

These are just a few examples of complex and differentiated TypeScript code that can be written. TypeScript is a powerful language that enables developers to write sophisticated and maintainable code.