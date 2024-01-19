Here is a complex and differentiated Rust code:

```
use std::io;
use std::collections::HashMap;
use std::fmt;

// Define a struct to represent a book.
#[derive(Debug)]
struct Book {
    title: String,
    author: String,
    year: u32,
}

// Define a function to read a book from the console.
fn read_book() -> Book {
    // Create a new book struct.
    let mut book = Book {
        title: String::new(),
        author: String::new(),
        year: 0,
    };

    // Read the book's title from the console.
    println!("Enter the book's title:");
    io::stdin().read_line(&mut book.title).unwrap();

    // Read the book's author from the console.
    println!("Enter the book's author:");
    io::stdin().read_line(&mut book.author).unwrap();

    // Read the book's year from the console.
    println!("Enter the book's year:");
    io::stdin().read_line(&mut book.year.to_string()).unwrap();

    // Convert the book's year from a string to a u32.
    book.year = book.year.trim().parse().unwrap();

    // Return the book.
    book
}

// Define a function to print a book to the console.
fn print_book(book: &Book) {
    println!("Title: {}", book.title);
    println!("Author: {}", book.author);
    println!("Year: {}", book.year);
}

// Define a function to add a book to a HashMap.
fn add_book(books: &mut HashMap<String, Book>, book: Book) {
    // Insert the book into the HashMap using its title as the key.
    books.insert(book.title.clone(), book);
}

// Define a function to remove a book from a HashMap.
fn remove_book(books: &mut HashMap<String, Book>, title: &str) {
    // Remove the book from the HashMap using its title as the key.
    books.remove(title);
}

// Define a function to search for a book in a HashMap.
fn search_book(books: &HashMap<String, Book>, title: &str) -> Option<&Book> {
    // Search for the book in the HashMap using its title as the key.
    books.get(title)
}

// Define a function to print all the books in a HashMap.
fn print_books(books: &HashMap<String, Book>) {
    // Iterate over all the books in the HashMap.
    for book in books.values() {
        // Print the book to the console.
        print_book(book);
    }
}

// Define the main function.
fn main() {
    // Create a new HashMap to store the books.
    let mut books: HashMap<String, Book> = HashMap::new();

    // Read a book from the console.
    let book1 = read_book();

    // Add the book to the HashMap.
    add_book(&mut books, book1);

    // Read another book from the console.
    let book2 = read_book();

    // Add the book to the HashMap.
    add_book(&mut books, book2);

    // Search for a book in the HashMap.
    let book = search_book(&books, "The Hitchhiker's Guide to the Galaxy");

    // Print the book to the console.
    match book {
        Some(book) => {
            println!("Book found:");
            print_book(book);
        },
        None => {
            println!("Book not found.");
        },
    }

    // Remove a book from the HashMap.
    remove_book(&mut books, "The Hitchhiker's Guide to the Galaxy");

    // Print all the books in the HashMap.
    println!("All books:");
    print_books(&books);
}
```

This code defines a `Book` struct to represent a book with a title, author, and year. It then defines several functions to read a book from the console, print a book to the console, add a book to a HashMap, remove a book from a HashMap, search for a book in a HashMap, and print all the books in a HashMap.

The `main` function creates a new HashMap to store the books, reads two books from the console, adds them to the HashMap, searches for a book in the HashMap, removes the book from the HashMap, and prints all the books in the HashMap.

This code is complex and differentiated because it uses a struct, a HashMap, and several functions to perform different operations on books. It also uses error handling to handle errors that may occur when reading from the console or when searching for a book in the HashMap.