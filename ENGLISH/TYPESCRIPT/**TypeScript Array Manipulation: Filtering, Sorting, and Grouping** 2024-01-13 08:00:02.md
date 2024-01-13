```typescript
// Define an interface for a Person
interface Person {
  name: string;
  age: number;
  occupation: string;
}

// Create an array of Person objects
const people: Person[] = [
  { name: 'John Doe', age: 30, occupation: 'Software Engineer' },
  { name: 'Jane Smith', age: 25, occupation: 'Marketing Manager' },
  { name: 'Michael Jones', age: 40, occupation: 'Doctor' },
  { name: 'Sarah Miller', age: 28, occupation: 'Accountant' },
  { name: 'Robert Brown', age: 35, occupation: 'Lawyer' },
];

// Function to filter people by their age
const filterPeopleByAge = (age: number) => {
  return people.filter((person) => person.age > age);
};

// Function to sort people by their name
const sortPeopleByName = () => {
  return people.sort((a, b) => a.name.localeCompare(b.name));
};

// Function to group people by their occupation
const groupPeopleByOccupation = () => {
  return people.reduce((acc, person) => {
    const occupation = person.occupation;
    if (!acc[occupation]) {
      acc[occupation] = [];
    }
    acc[occupation].push(person);
    return acc;
  }, {});
};

// Print the results of the filtering, sorting, and grouping functions
console.log(filterPeopleByAge(30)); // Output: [{ name: 'Michael Jones', age: 40, occupation: 'Doctor' }, { name: 'Robert Brown', age: 35, occupation: 'Lawyer' }]
console.log(sortPeopleByName()); // Output: [{ name: 'Jane Smith', age: 25, occupation: 'Marketing Manager' }, { name: 'John Doe', age: 30, occupation: 'Software Engineer' }, { name: 'Michael Jones', age: 40, occupation: 'Doctor' }, { name: 'Robert Brown', age: 35, occupation: 'Lawyer' }, { name: 'Sarah Miller', age: 28, occupation: 'Accountant' }]
console.log(groupPeopleByOccupation()); // Output: { 'Software Engineer': [{ name: 'John Doe', age: 30, occupation: 'Software Engineer' }], 'Marketing Manager': [{ name: 'Jane Smith', age: 25, occupation: 'Marketing Manager' }], 'Doctor': [{ name: 'Michael Jones', age: 40, occupation: 'Doctor' }], 'Accountant': [{ name: 'Sarah Miller', age: 28, occupation: 'Accountant' }], 'Lawyer': [{ name: 'Robert Brown', age: 35, occupation: 'Lawyer' }] }
```

Explanation:

1. We define an interface called `Person` which represents a person with properties `name`, `age`, and `occupation`.

2. We create an array of `Person` objects called `people`.

3. We define three functions:
   - `filterPeopleByAge` filters the `people` array by age, returning only those people who are older than a specified age.
   - `sortPeopleByName` sorts the `people` array by name in ascending order.
   - `groupPeopleByOccupation` groups the `people` array by occupation, returning an object with occupation names as keys and arrays of people with that occupation as values.

4. We call these functions and print the results to the console.

This code demonstrates various array manipulation techniques in TypeScript, including filtering, sorting, and grouping.