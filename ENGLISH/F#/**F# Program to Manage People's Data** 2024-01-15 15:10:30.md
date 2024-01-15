```f#
// Import the necessary libraries
open System
open System.Linq
open System.Collections.Generic

// Define a type to represent a person
type Person = { Name: string; Age: int }

// Define a function to create a person
let createPerson (name: string) (age: int) =
    { Name = name; Age = age }

// Define a function to get the names of all people
let getAllNames (people: Person[]) =
    people.Select(fun person -> person.Name)

// Define a function to get the average age of all people
let getAverageAge (people: Person[]) =
    people.Average(fun person -> person.Age)

// Define a function to find the oldest person
let findOldestPerson (people: Person[]) =
    people.MaxBy(fun person -> person.Age)

// Define a function to find the youngest person
let findYoungestPerson (people: Person[]) =
    people.MinBy(fun person -> person.Age)

// Create a list of people
let people = [
    createPerson "John" 20
    createPerson "Jane" 25
    createPerson "Joe" 30
    createPerson "Jill" 35
]

// Get the names of all people
let names = getAllNames people

// Get the average age of all people
let averageAge = getAverageAge people

// Find the oldest person
let oldestPerson = findOldestPerson people

// Find the youngest person
let youngestPerson = findYoungestPerson people

// Print the results
printfn "Names: %s" (String.Join ", ", names)
printfn "Average age: %f" averageAge
printfn "Oldest person: %s" oldestPerson.Name
printfn "Youngest person: %s" youngestPerson.Name
```

This code performs several operations on a list of people:

1. It defines a type called `Person` to represent a person with a name and age.
2. It defines a function called `createPerson` to create a person with a given name and age.
3. It defines a function called `getAllNames` to get the names of all people in a list.
4. It defines a function called `getAverageAge` to get the average age of all people in a list.
5. It defines a function called `findOldestPerson` to find the oldest person in a list.
6. It defines a function called `findYoungestPerson` to find the youngest person in a list.
7. It creates a list of people using the `createPerson` function.
8. It calls the `getAllNames`, `getAverageAge`, `findOldestPerson`, and `findYoungestPerson` functions on the list of people and prints the results.

This code demonstrates various functional programming concepts in F#, such as defining types, functions, using LINQ for list operations, and pattern matching.