```typescript
// This is a complex TypeScript code that demonstrates various advanced features of the language.

// Define an interface for a Person object.
interface Person {
  name: string;
  age: number;
  occupation: string;
}

// Create an array of Person objects.
const people: Person[] = [
  { name: "John Doe", age: 30, occupation: "Software Engineer" },
  { name: "Jane Smith", age: 25, occupation: "Doctor" },
  { name: "Michael Jones", age: 40, occupation: "Lawyer" },
];

// Define a function to filter people by their age.
const filterPeopleByAge = (people: Person[], age: number): Person[] => {
  return people.filter((person) => person.age > age);
};

// Define a function to get the oldest person from an array of people.
const getOldestPerson = (people: Person[]): Person => {
  return people.reduce((oldest, current) => (oldest.age > current.age ? oldest : current));
};

// Define a function to calculate the average age of an array of people.
const getAverageAge = (people: Person[]): number => {
  return people.reduce((sum, person) => sum + person.age, 0) / people.length;
};

// Define a function to group people by their occupation.
const groupPeopleByOccupation = (people: Person[]): Map<string, Person[]> => {
  const map = new Map<string, Person[]>();
  people.forEach((person) => {
    const occupation = person.occupation;
    if (!map.has(occupation)) {
      map.set(occupation, []);
    }
    map.get(occupation)?.push(person);
  });
  return map;
};

// Print the results.
console.log("People over 30:");
console.log(filterPeopleByAge(people, 30));

console.log("Oldest person:");
console.log(getOldestPerson(people));

console.log("Average age:");
console.log(getAverageAge(people));

console.log("People grouped by occupation:");
console.log(groupPeopleByOccupation(people));
```

This code demonstrates the following advanced TypeScript features:

* Interfaces: Interfaces are used to define the structure of objects. In this code, the `Person` interface is used to define the properties that a `Person` object should have.
* Arrays: Arrays are used to store collections of data. In this code, the `people` array is used to store an array of `Person` objects.
* Functions: Functions are used to perform specific tasks. In this code, the `filterPeopleByAge`, `getOldestPerson`, `getAverageAge`, and `groupPeopleByOccupation` functions are used to perform various operations on the `people` array.
* Arrow functions: Arrow functions are a concise way of writing functions. In this code, arrow functions are used to define the `filterPeopleByAge`, `getOldestPerson`, `getAverageAge`, and `groupPeopleByOccupation` functions.
* Generics: Generics are used to make functions and data structures more flexible and reusable. In this code, the `filterPeopleByAge`, `getOldestPerson`, `getAverageAge`, and `groupPeopleByOccupation` functions are generic, which means that they can be used with any type of data, not just `Person` objects.
* Maps: Maps are used to store key-value pairs. In this code, the `groupPeopleByOccupation` function uses a `Map` to group people by their occupation.
* The `reduce()` method: The `reduce()` method is used to reduce an array to a single value. In this code, the `reduce()` method is used to calculate the average age of the people in the `people` array.

This code is a good example of how TypeScript can be used to write complex and expressive code.