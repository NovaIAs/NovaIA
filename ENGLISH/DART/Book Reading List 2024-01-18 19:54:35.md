```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Define a class to represent a book.
class Book {
  final String title;
  final String author;
  final int yearPublished;

  Book(this.title, this.author, this.yearPublished);

  @override
  String toString() {
    return 'Book{title: $title, author: $author, yearPublished: $yearPublished}';
  }
}

// Define a function to load books from a JSON file.
Future<List<Book>> loadBooks() async {
  // Read the JSON file.
  final file = File('books.json');
  final json = await file.readAsString();

  // Parse the JSON string into a list of maps.
  final books = jsonDecode(json) as List<dynamic>;

  // Convert each map into a Book object.
  return books.map((book) {
    return Book(
      book['title'] as String,
      book['author'] as String,
      book['yearPublished'] as int,
    );
  }).toList();
}

// Define a function to print the books.
void printBooks(List<Book> books) {
  for (final book in books) {
    print(book);
  }
}

// Define the main function.
void main() async {
  // Load the books from the JSON file.
  final books = await loadBooks();

  // Print the books.
  printBooks(books);
}
```

This code defines a class called Book to represent a book. It has three properties: title, author, and yearPublished.

It also defines a function called loadBooks() that loads a list of books from a JSON file. The function first reads the JSON file, then parses the JSON string into a list of maps, and finally converts each map into a Book object.

The function printBooks() is then used to print the books to the console.

The main function calls the loadBooks() function and then calls the printBooks() function to print the books to the console.