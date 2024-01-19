```scala
// Bookshelf Library Management System

import scala.collection.mutable.ListBuffer

class Book(val title: String, val author: String, var status: String)

object Bookshelf {
  // Initialize the library with a few books
  val books = ListBuffer(
    Book("The Hitchhiker's Guide to the Galaxy", "Douglas Adams", "Available"),
    Book("The Catcher in the Rye", "J.D. Salinger", "Available"),
    Book("The Great Gatsby", "F. Scott Fitzgerald", "Available"),
    Book("1984", "George Orwell", "Available"),
    Book("Brave New World", "Aldous Huxley", "Available")
  )

  // Main menu of the library management system
  def mainMenu(): Unit = {
    println("Welcome to the Bookshelf Library Management System!")
    println("==============================================")
    println("1. List all books")
    println("2. Search for a book")
    println("3. Borrow a book")
    println("4. Return a book")
    println("5. Quit")
    print("Enter your choice: ")
    val choice = scala.io.StdIn.readInt()

    choice match {
      case 1 => listAllBooks()
      case 2 => searchBook()
      case 3 => borrowBook()
      case 4 => returnBook()
      case 5 => System.exit(0)
      case _ => println("Invalid choice. Please enter a number between 1 and 5.")
    }
    mainMenu() // Recursively call the main menu until the user quits
  }

  // List all books in the library
  def listAllBooks(): Unit = {
    println("List of all books:")
    books.foreach(book => println(s"${book.title} by ${book.author} - ${book.status}"))
  }

  // Search for a book by title or author
  def searchBook(): Unit = {
    print("Enter the title or author of the book you want to search for: ")
    val query = scala.io.StdIn.readLine()
    val results = books.filter(book => book.title.contains(query) || book.author.contains(query))
    if (results.isEmpty) {
      println("No results found.")
    } else {
      println("Search results:")
      results.foreach(book => println(s"${book.title} by ${book.author} - ${book.status}"))
    }
  }

  // Borrow a book
  def borrowBook(): Unit = {
    print("Enter the title of the book you want to borrow: ")
    val title = scala.io.StdIn.readLine()
    val book = books.find(_.title == title)
    if (book.isEmpty) {
      println("Book not found.")
    } else if (book.get.status == "Available") {
      book.get.status = "Borrowed"
      println(s"You have successfully borrowed ${book.get.title}.")
    } else {
      println("Book is not available for borrowing.")
    }
  }

  // Return a book
  def returnBook(): Unit = {
    print("Enter the title of the book you want to return: ")
    val title = scala.io.StdIn.readLine()
    val book = books.find(_.title == title)
    if (book.isEmpty) {
      println("Book not found.")
    } else if (book.get.status == "Borrowed") {
      book.get.status = "Available"
      println(s"You have successfully returned ${book.get.title}.")
    } else {
      println("Book is not borrowed.")
    }
  }

  // Start the library management system
  def main(args: Array[String]): Unit = {
    mainMenu()
  }
}
```

**Explanation:**

This code creates a simple library management system using Scala. It allows users to list all books, search for a book by title or author, borrow a book, and return a book.

The code defines a `Book` class with three properties: `title`, `author`, and `status`. The `status` property can be "Available" or "Borrowed."

The `Bookshelf` object contains a list of `Book` objects, representing the books in the library.

The `mainMenu()` method displays a menu of options to the user and calls the appropriate method based on the user's choice.

The `listAllBooks()` method lists all the books in the library.

The `searchBook()` method searches for a book by title or author and displays the results.

The `borrowBook()` method allows the user to borrow a book if it is available.

The `returnBook()` method allows the user to return a book if it is borrowed.

The `main()` method starts the library management system and calls the `mainMenu()` method.