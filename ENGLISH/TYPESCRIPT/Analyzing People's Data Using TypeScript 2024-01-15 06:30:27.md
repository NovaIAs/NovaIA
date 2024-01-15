```typescript
// Define an interface for a person
interface Person {
  name: string;
  age: number;
  occupation: string;
  interests: string[];
}

// Create an array of people
const people: Person[] = [
  {
    name: 'John Doe',
    age: 30,
    occupation: 'Software Engineer',
    interests: ['programming', 'hiking', 'reading']
  },
  {
    name: 'Jane Smith',
    age: 25,
    occupation: 'Doctor',
    interests: ['medicine', 'running', 'cooking']
  },
  {
    name: 'Michael Jones',
    age: 40,
    occupation: 'Lawyer',
    interests: ['law', 'golf', 'traveling']
  }
];

// Define a function to filter people by their age
const filterPeopleByAge = (people: Person[], age: number): Person[] => {
  return people.filter((person) => person.age > age);
};

// Define a function to get the occupations of people
const getOccupations = (people: Person[]): string[] => {
  return people.map((person) => person.occupation);
};

// Define a function to get the average age of people
const getAverageAge = (people: Person[]): number => {
  return people.reduce((sum, person) => sum + person.age, 0) / people.length;
};

// Define a function to get the most common interest among people
const getMostCommonInterest = (people: Person[]): string | undefined => {
  // Create a map to store the frequency of each interest
  const interestMap: Map<string, number> = new Map();

  // Iterate over the people and their interests
  for (const person of people) {
    for (const interest of person.interests) {
      // Increment the frequency of the interest in the map
      interestMap.set(interest, (interestMap.get(interest) ?? 0) + 1);
    }
  }

  // Find the interest with the highest frequency
  let mostCommonInterest: string | undefined;
  let highestFrequency = 0;
  for (const [interest, frequency] of interestMap) {
    if (frequency > highestFrequency) {
      mostCommonInterest = interest;
      highestFrequency = frequency;
    }
  }

  return mostCommonInterest;
};

// Log the results
console.log('People filtered by age (over 35):', filterPeopleByAge(people, 35));
console.log('Occupations of people:', getOccupations(people));
console.log('Average age of people:', getAverageAge(people));
console.log('Most common interest among people:', getMostCommonInterest(people));
```

Explanation:

1. We define an interface `Person` to represent a person with properties like name, age, occupation, and interests.

2. We create an array `people` containing three person objects with different information.

3. We define three functions:

   - `filterPeopleByAge` filters people by their age and returns a new array of people whose age is greater than a specified age.

   - `getOccupations` returns an array of occupations of people.

   - `getAverageAge` calculates and returns the average age of people.

4. We also define a function `getMostCommonInterest` which finds and returns the most common interest among people. This function uses a map to store the frequency of each interest and then finds the interest with the highest frequency.

5. Finally, we log the results of these functions to the console.