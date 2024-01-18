Here is a complex script in TypeScript that performs various operations on a list of objects. Comments and explanations are included for clarity:

```typescript
// Define an interface for the objects in the list
interface Person {
  name: string;
  age: number;
  occupation: string;
}

// Define a list of objects of type Person
const people: Person[] = [
  { name: 'John', age: 30, occupation: 'Software Engineer' },
  { name: 'Mary', age: 25, occupation: 'Doctor' },
  { name: 'Bob', age: 40, occupation: 'Teacher' },
  { name: 'Alice', age: 28, occupation: 'Lawyer' },
  { name: 'Tom', age: 35, occupation: 'Accountant' },
];

// 1. Find all people who are over 30 years old
const over30 = people.filter(person => person.age > 30);
console.log('People over 30: ', over30);

// 2. Find the total age of all people in the list
const totalAge = people.reduce((acc, person) => acc + person.age, 0);
console.log('Total age of all people: ', totalAge);

// 3. Find the person with the longest occupation
const longestOccupation = people.reduce((acc, person) => (acc.occupation.length < person.occupation.length ? person : acc), people[0]);
console.log('Person with the longest occupation: ', longestOccupation);

// 4. Group people by their occupation
const groupedByOccupation = people.reduce((acc, person) => {
  if (!acc[person.occupation]) {
    acc[person.occupation] = [];
  }
  acc[person.occupation].push(person);
  return acc;
}, {});
console.log('People grouped by occupation: ', groupedByOccupation);

// 5. Find the average age of people in each occupation
const averageAgeByOccupation = Object.keys(groupedByOccupation).reduce((acc, occupation) => {
  const totalAge = groupedByOccupation[occupation].reduce((acc, person) => acc + person.age, 0);
  const averageAge = totalAge / groupedByOccupation[occupation].length;
  acc[occupation] = averageAge;
  return acc;
}, {});
console.log('Average age by occupation: ', averageAgeByOccupation);

// 6. Sort people by their age in descending order
const sortedByAgeDescending = people.sort((a, b) => b.age - a.age);
console.log('People sorted by age in descending order: ', sortedByAgeDescending);

// 7. Create a new list of objects with only the name and occupation of each person
const nameAndOccupation = people.map(person => ({ name: person.name, occupation: person.occupation }));
console.log('List of objects with only name and occupation: ', nameAndOccupation);
```

This script performs various complex operations on a list of objects, such as filtering, aggregating, grouping, and sorting. It also demonstrates the use of higher-order functions like 'filter', 'reduce', and 'map' to manipulate and transform data in JavaScript.

That being said, the code is designed to be efficient, readable, and reusable for various data manipulation tasks. It leverages the power of TypeScript, JavaScript's strongly typed superset, to enforce data types and improve code reliability.