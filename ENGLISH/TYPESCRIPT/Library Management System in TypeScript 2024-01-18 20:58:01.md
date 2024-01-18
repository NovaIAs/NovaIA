```typescript
// Define an interface to represent a book
interface Book {
  title: string;
  author: string;
  year: number;
  isbn: string;
  price: number;
  tags: string[];
}

// Define a class to represent a library
class Library {
  // Define the books property to store the books in the library
  books: Book[];

  // Define the constructor to initialize the library with an initial set of books
  constructor(books: Book[]) {
    this.books = books;
  }

  // Define a method to add a book to the library
  addBook(book: Book) {
    this.books.push(book);
  }

  // Define a method to remove a book from the library
  removeBook(isbn: string) {
    // Find the index of the book to be removed
    const index = this.books.findIndex((book) => book.isbn === isbn);

    // If the book is found, remove it from the array
    if (index !== -1) {
      this.books.splice(index, 1);
    }
  }

  // Define a method to get all the books in the library
  getAllBooks(): Book[] {
    return this.books;
  }

  // Define a method to get all the books by a specific author
  getBooksByAuthor(author: string): Book[] {
    const authorBooks = this.books.filter((book) => book.author === author);
    return authorBooks;
  }

  // Define a method to get all the books published in a specific year
  getBooksByYear(year: number): Book[] {
    const yearBooks = this.books.filter((book) => book.year === year);
    return yearBooks;
  }

  // Define a method to get all the books with a specific tag
  getBooksByTag(tag: string): Book[] {
    // Filter the books by tags, returning only those that match the given tag
    const taggedBooks = this.books.filter((book) => book.tags.includes(tag));

    return taggedBooks;
  }

  // Define a method to get the most expensive book in the library
  getMostExpensiveBook(): Book | undefined {
    // Sort the books by price in descending order
    const sortedBooks = this.books.sort((a, b) => b.price - a.price);

    // Return the first book in the sorted array (the most expensive one)
    return sortedBooks[0];
  }

  // Define a method to get the cheapest book in the library
  getCheapestBook(): Book | undefined {
    // Sort the books by price in ascending order
    const sortedBooks = this.books.sort((a, b) => a.price - b.price);

    // Return the first book in the sorted array (the cheapest one)
    return sortedBooks[0];
  }
}

// Create a new library object
const library = new Library([
  {
    title: "The Iliad",
    author: "Homer",
    year: 800,
    isbn: "978-0140444445",
    price: 12.99,
    tags: ["epic poetry", "ancient greece"],
  },
  {
    title: "The Catcher in the Rye",
    author: "J. D. Salinger",
    year: 1951,
    isbn: "978-0316769174",
    price: 8.99,
    tags: ["coming-of-age", "american literature"],
  },
  {
    title: "The Great Gatsby",
    author: "F. Scott Fitzgerald",
    year: 1925,
    isbn: "978-0743273565",
    price: 10.99,
    tags: ["jazz age", "american literature"],
  },
  {
    title: "1984",
    author: "George Orwell",
    year: 1949,
    isbn: "978-0451524935",
    price: 9.99,
    tags: ["dystopian", "political fiction"],
  },
  {
    title: "Pride and Prejudice",
    author: "Jane Austen",
    year: 1813,
    isbn: "978-0679405429",
    price: 12.99,
    tags: ["romance", "classic literature"],
  },
]);

// Add a new book to the library
library.addBook({
  title: "To Kill a Mockingbird",
  author: "Harper Lee",
  year: 1960,
  isbn: "978-0062420701",
  price: 15.99,
  tags: ["coming-of-age", "american literature"],
});

// Remove a book from the library
library.removeBook("978-0451524935");

// Get all the books in the library
const allBooks = library.getAllBooks();
console.log("All books in the library:");
console.table(allBooks);

// Get all the books by a specific author
const authorBooks = library.getBooksByAuthor("Jane Austen");
console.log("Books by Jane Austen:");
console.table(authorBooks);

// Get all the books published in a specific year
const yearBooks = library.getBooksByYear(1951);
console.log("Books published in 1951:");
console.table(yearBooks);

// Get all the books with a specific tag
const taggedBooks = library.getBooksByTag("coming-of-age");
console.log("Books with the tag 'coming-of-age':");
console.table(taggedBooks);

// Get the most expensive book in the library
const mostExpensiveBook = library.getMostExpensiveBook();
console.log("Most expensive book in the library:");
console.log(mostExpensiveBook);

// Get the cheapest book in the library
const cheapestBook = library.getCheapestBook();
console