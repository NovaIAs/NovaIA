```f#
// Import necessary libraries
open System
open System.Linq
open System.Collections.Generic

// Define a custom type to represent a person
type Person = {
    Name: string
    Age: int
    Gender: char
}

// Define a custom type to represent a pet
type Pet = {
    Name: string
    Type: string
    Age: int
}

// Define a custom type to represent a household
type Household = {
    People: List<Person>
    Pets: List<Pet>
    Address: string
}

// Define a function to create a new person
let createPerson (name: string) (age: int) (gender: char) = {
    Name = name
    Age = age
    Gender = gender
}

// Define a function to create a new pet
let createPet (name: string) (petType: string) (age: int) = {
    Name = name
    Type = petType
    Age = age
}

// Define a function to create a new household
let createHousehold (people: List<Person>) (pets: List<Pet>) (address: string) = {
    People = people
    Pets = pets
    Address = address
}

// Create a list of people
let people = [
    createPerson "John" 30 'M'
    createPerson "Mary" 25 'F'
    createPerson "Bob" 8 'M'
]

// Create a list of pets
let pets = [
    createPet "Fluffy" "Cat" 5
    createPet "Fido" "Dog" 3
]

// Create a list of households
let households = [
    createHousehold people pets "123 Main Street"
]

// Print the details of each household
for household in households do
    printfn "%s" household.Address
    printfn "People:"
    for person in household.People do
        printfn "\t%s (%d years old, %c)" person.Name person.Age person.Gender
    printfn "Pets:"
    for pet in household.Pets do
        printfn "\t%s (%s, %d years old)" pet.Name pet.Type pet.Age
```

Explanation:

1. We first define custom types to represent `Person`, `Pet`, and `Household`. These types allow us to store and manipulate data related to people, pets, and households in a structured manner.


2. We then define functions to create new `Person`, `Pet`, and `Household` instances. These functions take the necessary parameters and return a new instance of the corresponding type.


3. We create lists of `Person`, `Pet`, and `Household` instances using the functions we defined earlier. These lists represent the data for our households.


4. Finally, we use a `for` loop to iterate over the list of households and print the details of each household, including the names, ages, and genders of the people living in the household, as well as the names, types, and ages of the pets in the household.