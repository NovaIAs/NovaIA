```smalltalk
"Module for managing a library of books."

Class Book {
    "A book in the library."
    fields {
        title: String;
        author: String;
        isbn: String;
        copies: Integer;
    }

    methods {
        initialize: aTitle anAuthor anISBN aCopies [
            "Create a new book."
            title := aTitle;
            author := anAuthor;
            isbn := anISBN;
            copies := aCopies
        ]

        getTitle [
            "Return the title of the book."
            ^ title
        ]

        getAuthor [
            "Return the author of the book."
            ^ author
        ]

        getISBN [
            "Return the ISBN of the book."
            ^ isbn
        ]

        getCopies [
            "Return the number of copies of the book."
            ^ copies
        ]

        incrementCopies [
            "Increment the number of copies of the book."
            copies := copies + 1
        ]

        decrementCopies [
            "Decrement the number of copies of the book."
            copies := copies - 1
        ]
    }
}

Class Library {
    "A library of books."
    fields {
        books: Collection;
    }

    methods {
        initialize [
            "Create a new library."
            books := Collection new
        ]

        addBook: aBook [
            "Add a book to the library."
            books add: aBook
        ]

        removeBook: aBook [
            "Remove a book from the library."
            books remove: aBook
        }

        findBookByTitle: aTitle [
            "Find a book by its title."
            ^ books detect: [:book | book getTitle = aTitle]
        }

        findBookByAuthor: anAuthor [
            "Find a book by its author."
            ^ books detect: [:book | book getAuthor = anAuthor]
        }

        findBookByISBN: anISBN [
            "Find a book by its ISBN."
            ^ books detect: [:book | book getISBN = anISBN]
        }

        listAllBooks [
            "List all the books in the library."
            books do: [:book | println: book]
        ]
    }
}

"Create a library and add some books to it."
aLibrary := Library new.
aLibrary addBook: Book new initialize: 'To Kill a Mockingbird' 'Harper Lee' '0062420701' 5.
aLibrary addBook: Book new initialize: 'The Great Gatsby' 'F. Scott Fitzgerald' '0743273567' 3.
aLibrary addBook: Book new initialize: '1984' 'George Orwell' '0451524934' 2.

"Find a book by its title."
aBook := aLibrary findBookByTitle: '1984'.

"Increment the number of copies of the book."
aBook incrementCopies.

"List all the books in the library."
aLibrary listAllBooks.
```

This code defines a module for managing a library of books. It includes two classes, `Book` and `Library`, which represent books and a library, respectively. The `Book` class has fields for the title, author, ISBN, and number of copies of the book. The `Library` class has a field for the collection of books in the library.

The `Book` class has methods for getting and setting the title, author, ISBN, and number of copies. It also has methods for incrementing and decrementing the number of copies.

The `Library` class has methods for adding and removing books, finding books by title, author, or ISBN, and listing all the books in the library.

The main part of the code creates a library and adds some books to it. It then finds a book by its title, increments the number of copies, and lists all the books in the library.