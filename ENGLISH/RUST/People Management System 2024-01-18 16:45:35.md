```rust
// Define a struct to represent a person
#[derive(Debug)]
struct Person {
    name: String,
    age: u8,
    occupation: String,
}


// Define a function to create a new person
fn create_person(name: &str, age: u8, occupation: &str) -> Person {
    Person {
        name: name.to_string(),
        age,
        occupation: occupation.to_string(),
    }
}

// Define a function to print the details of a person
fn print_person(person: &Person) {
    println!("Name: {}", person.name);
    println!("Age: {}", person.age);
    println!("Occupation: {}", person.occupation);
}

// Define a function to find the oldest person in a list of people
fn find_oldest_person(people: &[Person]) -> Option<&Person> {
    let oldest_person = people.iter().max_by(|a, b| a.age.cmp(&b.age));
    oldest_person
}

// Define a function to find the youngest person in a list of people
fn find_youngest_person(people: &[Person]) -> Option<&Person> {
    let youngest_person = people.iter().min_by(|a, b| a.age.cmp(&b.age));
    youngest_person
}


// Main function
fn main() {
    // Create a list of people
    let people = vec![
        create_person("John", 30, "Software Engineer"),
        create_person("Mary", 25, "Doctor"),
        create_person("Bob", 40, "Teacher"),
        create_person("Alice", 20, "Student"),
    ];

    // Print the details of each person
    for person in &people {
        print_person(person);
        println!();
    }

    // Find the oldest and youngest people in the list
    let oldest_person = find_oldest_person(&people).unwrap();
    let youngest_person = find_youngest_person(&people).unwrap();

    // Print the details of the oldest and youngest people
    println!("Oldest Person:");
    print_person(oldest_person);
    println!();
    println!("Youngest Person:");
    print_person(youngest_person);
}
```
This code defines a struct `Person` to represent a person with fields for `name`, `age`, and `occupation`. It provides various functions to create a new person, print the details of a person, and find the oldest and youngest person in a list of people.
The main function creates a list of people and then prints the details of each person. It finally finds the oldest and youngest person in the list and prints their details.