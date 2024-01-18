```kotlin

// Define a class to represent a library book
class Book(val title: String, val author: String, val year: Int) {

    // Override the `toString()` method to provide a custom string representation of the book
    override fun toString(): String {
        return "$title by $author ($year)"
    }
}

// Define a class to represent a library
class Library(val books: MutableList<Book> = mutableListOf()) {

    // Add a book to the library
    fun addBook(book: Book) {
        books.add(book)
    }

    // Remove a book from the library
    fun removeBook(book: Book) {
        books.remove(book)
    }

    // Find a book in the library by its title
    fun findBookByTitle(title: String): Book? {
        return books.find { it.title == title }
    }

    // Find a book in the library by its author
    fun findBookByAuthor(author: String): List<Book> {
        return books.filter { it.author == author }
    }

    // Find all books in the library published in a given year
    fun findBooksByYear(year: Int): List<Book> {
        return books.filter { it.year == year }
    }

    // Sort the books in the library by title
    fun sortByTitle() {
        books.sortBy { it.title }
    }

    // Sort the books in the library by author
    fun sortByAuthor() {
        books.sortBy { it.author }
    }

    // Sort the books in the library by year
    fun sortByYear() {
        books.sortBy { it.year }
    }

    // Print the books in the library
    fun printBooks() {
        books.forEach { println(it) }
    }
}

// Create a library object
val library = Library()

// Add some books to the library
library.addBook(Book("The Iliad", "Homer", -800))
library.addBook(Book("The Odyssey", "Homer", -700))
library.addBook(Book("The Aeneid", "Virgil", -29))
library.addBook(Book("The Divine Comedy", "Dante Alighieri", 1308))
library.addBook(Book("Hamlet", "William Shakespeare", 1603))
library.addBook(Book("Pride and Prejudice", "Jane Austen", 1813))
library.addBook(Book("Moby Dick", "Herman Melville", 1851))
library.addBook(Book("The Adventures of Huckleberry Finn", "Mark Twain", 1884))
library.addBook(Book("The Great Gatsby", "F. Scott Fitzgerald", 1925))
library.addBook(Book("To Kill a Mockingbird", "Harper Lee", 1960))

// Print the books in the library
println("All books:")
library.printBooks()

// Find a book by its title
val book = library.findBookByTitle("Hamlet")
println("Book found by title \"Hamlet\":")
println(book)

// Find books by their author
val books = library.findBookByAuthor("Homer")
println("Books found by author \"Homer\":")
books.forEach { println(it) }

// Find books published in a given year
val books = library.findBooksByYear(1813)
println("Books found published in 1813:")
books.forEach { println(it) }

// Sort the books by title
library.sortByTitle()
println("Books sorted by title:")
library.printBooks()

// Sort the books by author
library.sortByAuthor()
println("Books sorted by author:")
library.printBooks()

// Sort the books by year
library.sortByYear()
println("Books sorted by year:")
library.printBooks()

```

This code defines a `Book` class to represent a library book, with properties for the title, author, and year of publication. It also defines a `Library` class to represent a collection of books, with methods to add, remove, find, and sort books. The code then creates a library object and adds some books to it. It then demonstrates how to use the library's methods to find and sort books.