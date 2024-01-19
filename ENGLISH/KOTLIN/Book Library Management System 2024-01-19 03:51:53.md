```kotlin
// Define an inline function to convert string to integer with default value
inline fun stringToIntOrDefault(s: String, default: Int): Int {
    return try {
        s.toInt()
    } catch (e: NumberFormatException) {
        default
    }
}

// Define a class to represent a book
class Book(val title: String, val author: String, val yearOfPublication: Int) {
    // Override the toString method to provide a meaningful representation of the book
    override fun toString(): String {
        return "Title: $title, Author: $author, Year: $yearOfPublication"
    }
}

// Define a class to represent a library
class Library(val books: MutableList<Book>) {
    // Add a book to the library
    fun addBook(book: Book) {
        books.add(book)
    }

    // Find a book by its title and author
    fun findBook(title: String, author: String): Book? {
        return books.find { it.title == title && it.author == author }
    }

    // Remove a book from the library
    fun removeBook(title: String, author: String): Boolean {
        return books.removeIf { it.title == title && it.author == author }
    }
}

// Create a library object
val library = Library(mutableListOf())

// Add some books to the library
library.addBook(Book("The Hitchhiker's Guide to the Galaxy", "Douglas Adams", 1979))
library.addBook(Book("The Lord of the Rings", "J.R.R. Tolkien", 1954))
library.addBook(Book("To Kill a Mockingbird", "Harper Lee", 1960))

// Find a book by its title and author
val foundBook = library.findBook("The Lord of the Rings", "J.R.R. Tolkien")

// Print the found book
println("Found book: $foundBook")

// Remove a book from the library
library.removeBook("To Kill a Mockingbird", "Harper Lee")

// Print the remaining books in the library
println("Remaining books:")
library.books.forEach { println(it) }
```

Explanation:

1. We start by defining an inline function called `stringToIntOrDefault`, which converts a string to an integer. It takes a string and a default value as parameters, and returns the integer value of the string if it's a valid number, or the default value if the string is not a valid number.

2. Next, we define a class called `Book` to represent a book. It has three properties: `title`, `author`, and `yearOfPublication`. It also overrides the `toString` method to provide a meaningful representation of the book.

3. We then define a class called `Library` to represent a library. It has a single property called `books`, which is a mutable list of `Book` objects. It provides methods to add, find, and remove books from the library.

4. We create a `Library` object called `library` and add some books to it using the `addBook` method.

5. We find a book by its title and author using the `findBook` method.

6. We print the found book.

7. We remove a book from the library using the `removeBook` method.

8. Finally, we print the remaining books in the library using a `forEach` loop.