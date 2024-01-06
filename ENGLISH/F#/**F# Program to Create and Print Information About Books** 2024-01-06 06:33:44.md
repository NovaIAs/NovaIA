```f#
// Define a custom type 'Book' with properties 'Title', 'Author', and 'ISBN'.
type Book = {
  Title: string;
  Author: string;
  ISBN: string;
}

// Define a function 'CreateBook' that takes title, author, and ISBN as arguments and returns a new 'Book' object.
let CreateBook (title: string) (author: string) (isbn: string) =
  {
    Title = title;
    Author = author;
    ISBN = isbn;
  }

// Define a list of books using the 'CreateBook' function.
let books = [
  CreateBook "The Hitchhiker's Guide to the Galaxy" "Douglas Adams" "0345391802";
  CreateBook "The Lord of the Rings" "J.R.R. Tolkien" "0395082560";
  CreateBook "Pride and Prejudice" "Jane Austen" "0679405429";
]

// Define a function 'PrintBookInfo' that takes a 'Book' object as an argument and prints its information.
let PrintBookInfo book =
  printfn "%s by %s (ISBN: %s)" book.Title book.Author book.ISBN

// Iterate over the list of books and print information about each book.
List.iter PrintBookInfo books
```

Explanation:

1. We define a custom type called 'Book' with three properties: 'Title', 'Author', and 'ISBN'. This type represents a book with its title, author, and ISBN number.

2. We define a function called 'CreateBook' that takes three arguments: 'title', 'author', and 'isbn'. This function creates and returns a new 'Book' object with the provided values.

3. We create a list of books using the 'CreateBook' function. Each element in the list is a 'Book' object representing a different book.

4. We define a function called 'PrintBookInfo' that takes a 'Book' object as an argument. This function prints information about the book, including its title, author, and ISBN.

5. We iterate over the list of books using the 'List.iter' function and call the 'PrintBookInfo' function on each book to print its information.

This code demonstrates the use of custom types, functions, and list processing in F#. It shows how to create custom data structures, manipulate data, and print information about the data in a structured manner.