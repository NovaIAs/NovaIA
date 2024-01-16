```typescript
// Define an interface for a person
interface Person {
  name: string;
  age: number;
  gender: string;
  occupation: string;
}

// Create an array of people
const people: Person[] = [
  { name: 'John Doe', age: 30, gender: 'male', occupation: 'software engineer' },
  { name: 'Jane Smith', age: 25, gender: 'female', occupation: 'doctor' },
  { name: 'Michael Jones', age: 40, gender: 'male', occupation: 'lawyer' },
  { name: 'Sarah Miller', age: 35, gender: 'female', occupation: 'teacher' },
  { name: 'David Williams', age: 28, gender: 'male', occupation: 'accountant' },
];

// Create a function to filter people by their occupation
const filterPeopleByOccupation = (occupation: string): Person[] => {
  return people.filter((person) => person.occupation === occupation);
};

// Create a function to get the average age of people in a given array
const getAverageAge = (people: Person[]): number => {
  const totalAge = people.reduce((acc, person) => acc + person.age, 0);
  return totalAge / people.length;
};

// Create a function to group people by their gender
const groupPeopleByGender = (people: Person[]): Record<string, Person[]> => {
  return people.reduce((acc, person) => {
    if (acc[person.gender]) {
      acc[person.gender].push(person);
    } else {
      acc[person.gender] = [person];
    }
    return acc;
  }, {} as Record<string, Person[]>);
};

// Create a function to sort people by their age in ascending order
const sortByAge = (people: Person[]): Person[] => {
  return people.sort((a, b) => a.age - b.age);
};

// Create a function to find the oldest person in a given array
const findOldestPerson = (people: Person[]): Person => {
  return people.reduce((oldest, person) => (person.age > oldest.age ? person : oldest), people[0]);
};

// Create a function to find the youngest person in a given array
const findYoungestPerson = (people: Person[]): Person => {
  return people.reduce((youngest, person) => (person.age < youngest.age ? person : youngest), people[0]);
};

// Create a function to get the names of all people in a given array
const getNames = (people: Person[]): string[] => {
  return people.map((person) => person.name);
};

// Create a function to get the occupations of all people in a given array
const getOccupations = (people: Person[]): string[] => {
  return people.map((person) => person.occupation);
};

// Create a function to get the genders of all people in a given array
const getGenders = (people: Person[]): string[] => {
  return people.map((person) => person.gender);
};

// Create a function to get the ages of all people in a given array
const getAges = (people: Person[]): number[] => {
  return people.map((person) => person.age);
};

// Export the functions
export {
  filterPeopleByOccupation,
  getAverageAge,
  groupPeopleByGender,
  sortByAge,
  findOldestPerson,
  findYoungestPerson,
  getNames,
  getOccupations,
  getGenders,
  getAges,
};
```

Explanation:

This code defines a TypeScript module that exports a set of functions for working with an array of people. The module defines an interface for a person, which includes properties for the person's name, age, gender, and occupation. It then defines an array of people that conforms to this interface.

The module then defines a set of functions that can be used to perform various operations on the array of people. These functions include:

* `filterPeopleByOccupation`: This function takes an occupation as an argument and returns an array of people who have that occupation.
* `getAverageAge`: This function takes an array of people as an argument and returns the average age of the people in the array.
* `groupPeopleByGender`: This function takes an array of people as an argument and returns an object where the keys are the genders of the people and the values are arrays of people who have that gender.
* `sortByAge`: This function takes an array of people as an argument and returns an array of people sorted by their age in ascending order.
* `findOldestPerson`: This function takes an array of people as an argument and returns the oldest person in the array.
* `findYoungestPerson`: This function takes an array of people as an argument and returns the youngest person in the array.
* `getNames`: This function takes an array of people as an argument and returns an array of the names of the people in the array.
* `getOccupations`: This function takes an array of people as an argument and returns an array of the occupations of the people in the array.
* `getGenders`: This function takes an array of people as an argument and returns an array of the genders of the people in the array.
* `getAges`: This function takes an array of people as an argument and returns an array of the ages of the people in the array.

These functions can be used to perform a variety of tasks, such as filtering people by their occupation, getting the average age of a group of people, or finding the oldest or youngest person in a group.

To use the module, you can import it into your TypeScript file using the following code:

```typescript
import {
  filterPeopleByOccupation,
  getAverageAge,
  groupPeopleByGender,
  sortByAge,
  findOldestPerson,
  findYoungestPerson,
  getNames,
  getOccupations,
  getGenders,
  getAges,
} from './people';
```

You can then use the functions exported by the module to perform the desired operations on your array of people. For example, the following code uses the `filterPeopleByOccupation` function to filter the array of people by the occupation "software engineer":

```typescript
const softwareEngineers = filterPeopleByOccupation('software engineer');
```

The `softwareEngineers` variable will now contain an array of all the people in the `people` array who are software engineers.