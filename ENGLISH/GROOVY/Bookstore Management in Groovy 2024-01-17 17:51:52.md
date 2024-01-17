```groovy
// Define a class to represent a book.
class Book {

    // Properties of the book.
    String title
    String author
    int pages
    double price

    // Constructor to initialize the properties.
    Book(String title, String author, int pages, double price) {
        this.title = title
        this.author = author
        this.pages = pages
        this.price = price
    }

    // Method to print the details of the book.
    def printDetails() {
        println "Title: $title"
        println "Author: $author"
        println "Pages: $pages"
        println "Price: $price"
    }
}

// Create a list of books.
def books = [
    new Book("The Lord of the Rings", "J.R.R. Tolkien", 1207, 30.99),
    new Book("The Iliad", "Homer", 683, 12.99),
    new Book("The Great Gatsby", "F. Scott Fitzgerald", 180, 10.99),
    new Book("1984", "George Orwell", 328, 9.99),
    new Book("To Kill a Mockingbird", "Harper Lee", 324, 15.99)
]

// Print the details of each book.
books.each { book -> book.printDetails() }

// Find the book with the highest price.
def maxPriceBook = books.max { it.price }

// Print the details of the book with the highest price.
println "Book with the highest price:"
maxPriceBook.printDetails()

// Find the total price of all the books.
def totalPrice = books.sum { it.price }

// Print the total price of all the books.
println "Total price of all books: $totalPrice"

// Group the books by author.
def booksByAuthor = books.groupBy { it.author }

// Print the books grouped by author.
booksByAuthor.each { author, books ->
    println "Books by $author:"
    books.each { book -> book.printDetails() }
}

// Filter the books to include only those with more than 500 pages.
def longBooks = books.findAll { it.pages > 500 }

// Print the long books.
println "Long books:"
longBooks.each { book -> book.printDetails() }

// Sort the books by price in descending order.
def sortedBooks = books.sort { -it.price }

// Print the sorted books.
println "Sorted books by price in descending order:"
sortedBooks.each { book -> book.printDetails() }
```

This Groovy code performs various operations on a list of books:

1. It defines a `Book` class with properties like `title`, `author`, `pages`, and `price`.

2. It creates a list of `Book` objects representing different books with their details.

3. It prints the details of each book using the `printDetails()` method.

4. It finds the book with the highest price using the `max()` function and prints its details.

5. It calculates the total price of all the books using the `sum()` function.

6. It groups the books by author using the `groupBy()` function and prints the books for each author.

7. It filters the books to include only those with more than 500 pages using the `findAll()` function.

8. It sorts the books by price in descending order using the `sort()` function.

9. It prints the sorted books.

This code demonstrates various Groovy features such as classes, objects, methods, collections, lambdas, and closures. It also showcases the use of Groovy's concise and expressive syntax.