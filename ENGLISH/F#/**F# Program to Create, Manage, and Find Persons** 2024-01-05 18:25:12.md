```f#
// Create a custom type to represent a person
type Person =
    { Name : string
      Age : int
      Occupation : string }

// Define a function to create a new person
let createPerson name age occupation =
    { Name = name
      Age = age
      Occupation = occupation }

// Define a function to print the details of a person
let printPerson person =
    printfn "%s is %d years old and works as a %s" person.Name person.Age person.Occupation

// Create a list of people
let people =
    [ createPerson "John Doe" 25 "Software Engineer"
      createPerson "Jane Smith" 30 "Doctor"
      createPerson "Bill Jones" 40 "Lawyer" ]

// Iterate over the list of people and print their details
List.iter printPerson people

// Define a function to find a person by name
let findPersonByName name people =
    List.find (fun person -> person.Name = name) people

// Find a person by name and print their details
let person = findPersonByName "John Doe" people
printPerson person
```

This code defines a custom type called `Person` to represent a person with a name, age, and occupation. It also defines functions to create a new person, print the details of a person, find a person by name, and a list of people. The code then iterates over the list of people and prints their details. Finally, it finds a person by name and prints their details.