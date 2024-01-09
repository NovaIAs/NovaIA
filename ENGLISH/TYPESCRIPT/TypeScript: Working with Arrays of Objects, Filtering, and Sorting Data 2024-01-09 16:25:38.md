```typescript
// Define an interface for a person
interface Person {
  name: string;
  age: number;
  occupation: string;
}

// Create an array of people
const people: Person[] = [
  { name: 'John Doe', age: 30, occupation: 'Software Engineer' },
  { name: 'Jane Smith', age: 25, occupation: 'Doctor' },
  { name: 'Michael Jones', age: 40, occupation: 'Lawyer' },
];

// Create a function to filter people by age
const filterPeopleByAge = (age: number) => {
  return people.filter((person) => person.age > age);
};

// Create a function to filter people by occupation
const filterPeopleByOccupation = (occupation: string) => {
  return people.filter((person) => person.occupation === occupation);
};

// Create a function to sort people by age
const sortPeopleByAge = () => {
  return people.sort((a, b) => a.age - b.age);
};

// Create a function to sort people by name
const sortPeopleByName = () => {
  return people.sort((a, b) => a.name.localeCompare(b.name));
};

// Create a function to print the details of a person
const printPersonDetails = (person: Person) => {
  console.log(`Name: ${person.name}`);
  console.log(`Age: ${person.age}`);
  console.log(`Occupation: ${person.occupation}`);
};

// Filter people by age and print their details
const filteredPeopleByAge = filterPeopleByAge(35);
console.log('People older than 35:');
filteredPeopleByAge.forEach((person) => printPersonDetails(person));

// Filter people by occupation and print their details
const filteredPeopleByOccupation = filterPeopleByOccupation('Doctor');
console.log('People who are doctors:');
filteredPeopleByOccupation.forEach((person) => printPersonDetails(person));

// Sort people by age and print their details
const sortedPeopleByAge = sortPeopleByAge();
console.log('People sorted by age:');
sortedPeopleByAge.forEach((person) => printPersonDetails(person));

// Sort people by name and print their details
const sortedPeopleByName = sortPeopleByName();
console.log('People sorted by name:');
sortedPeopleByName.forEach((person) => printPersonDetails(person));
```

Explanation:

1. We define an interface `Person` to represent a person with properties `name`, `age`, and `occupation`.
2. We create an array of people using the `Person` interface.
3. We define functions to filter people by age and occupation.
4. We define functions to sort people by age and name.
5. We define a function to print the details of a person.
6. We use the filter functions to filter people by age and occupation and print their details.
7. We use the sort functions to sort people by age and name and print their details.