```groovy
import groovy.json.JsonSlurper

// Define a class to represent a book
class Book {
    String title
    String author
    int year
    String isbn
}

// Define a class to represent a library
class Library {
    List<Book> books = []

    // Method to add a book to the library
    void addBook(Book book) {
        books.add(book)
    }

    // Method to get all books in the library
    List<Book> getAllBooks() {
        return books
    }

    // Method to get all books by a specific author
    List<Book> getBooksByAuthor(String author) {
        return books.findAll { it.author == author }
    }

    // Method to get all books published in a specific year
    List<Book> getBooksByYear(int year) {
        return books.findAll { it.year == year }
    }

    // Method to get all books with a specific ISBN
    Book getBookByIsbn(String isbn) {
        return books.find { it.isbn == isbn }
    }
}

// Create a new library
def library = new Library()

// Add some books to the library
library.addBook(new Book(title: "The Lord of the Rings", author: "J.R.R. Tolkien", year: 1954, isbn: "9780395082560"))
library.addBook(new Book(title: "The Hobbit", author: "J.R.R. Tolkien", year: 1937, isbn: "9780395071222"))
library.addBook(new Book(title: "The Catcher in the Rye", author: "J.D. Salinger", year: 1951, isbn: "9780316769174"))
library.addBook(new Book(title: "The Great Gatsby", author: "F. Scott Fitzgerald", year: 1925, isbn: "9780743273565"))
library.addBook(new Book(title: "1984", author: "George Orwell", year: 1949, isbn: "9780451524935"))

// Get all books in the library
def allBooks = library.getAllBooks()
println("All books in the library:")
allBooks.each { println it.title }

// Get all books by J.R.R. Tolkien
def tolkienBooks = library.getBooksByAuthor("J.R.R. Tolkien")
println("Books by J.R.R. Tolkien:")
tolkienBooks.each { println it.title }

// Get all books published in 1954
def booksFrom1954 = library.getBooksByYear(1954)
println("Books published in 1954:")
booksFrom1954.each { println it.title }

// Get the book with ISBN 9780395071222
def bookWithIsbn = library.getBookByIsbn("9780395071222")
println("Book with ISBN 9780395071222:")
println bookWithIsbn.title

// Load a JSON file containing book data
def jsonSlurper = new JsonSlurper()
def bookData = jsonSlurper.parseText(new File("books.json").text)

// Add the books from the JSON file to the library
bookData.each { book ->
    library.addBook(new Book(title: book.title, author: book.author, year: book.year, isbn: book.isbn))
}

// Get all books in the library again
allBooks = library.getAllBooks()
println("All books in the library after adding books from JSON:")
allBooks.each { println it.title }
```

**Explanation:**

This Groovy code defines a class called `Book` to represent a book with properties such as title, author, year, and ISBN. It also defines a class called `Library` to represent a collection of books, with methods to add books, get all books, get books by author, get books by year, and get a book by ISBN.

The code then creates a new library and adds some books to it. It then demonstrates how to use the library's methods to get all books, get books by author, get books by year, and get a book by ISBN.

Finally, the code loads a JSON file containing book data and adds the books from the JSON file to the library. It then demonstrates how to get all books in the library again after adding the books from the JSON file.

This code is complex and differentiated because it demonstrates a variety of Groovy features, including classes, methods, loops, conditional statements, and file I/O. It also demonstrates how to load and parse JSON data using the `JsonSlurper` class.