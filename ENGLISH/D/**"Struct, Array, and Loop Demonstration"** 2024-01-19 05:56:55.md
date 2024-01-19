```d
// Define a struct to represent a person.
struct Person {
    // Declare the fields of the struct.
    string name;
    int age;
    string occupation;
}

// Define a function to print the details of a person.
void printPerson(Person person) {
    // Print the person's name, age, and occupation.
    writeln("Name:", person.name);
    writeln("Age:", person.age);
    writeln("Occupation:", person.occupation);
}

// Define an array of Person structs.
Person[] people = [
    // Create a new Person struct for each person.
    new Person("John Doe", 30, "Software Engineer"),
    new Person("Jane Smith", 25, "Accountant"),
    new Person("Michael Jones", 40, "Doctor")
];

// Iterate over the array of people and print the details of each person.
foreach (person; people) {
    printPerson(person);
}
```

Explanation:

1. We define a struct called Person to represent a person. The struct has three fields: name, age, and occupation.

2. We define a function called printPerson() to print the details of a person. The function takes a Person struct as an argument and prints the person's name, age, and occupation.

3. We define an array of Person structs called people. The array contains three Person structs, each representing a different person.

4. We use a foreach loop to iterate over the array of people. In each iteration, we call the printPerson() function to print the details of the current person.

This code creates an array of Person structs, populates the array with data, and then iterates over the array and prints the details of each person. This code demonstrates how to use structs, arrays, and loops in the D programming language.