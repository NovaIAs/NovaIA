```smalltalk
// Class definition for a Book
class Book
  attributes: title author year pages

  // Class methods
  class methods:
    new: [aTitle anAuthor aYear somePages]
      ^self new set: { title: aTitle; author: anAuthor; year: aYear; pages: somePages }.

    // Instance methods
  methods:
    title: ^title.
    author: ^author.
    year: ^year.
    pages: ^pages.

    printBookInfo:
      "[Print the book information]"
      Transcript show: 'Title: ', title; cr.
      Transcript show: 'Author: ', author; cr.
      Transcript show: 'Year: ', year; cr.
      Transcript show: 'Pages: ', pages; cr.
      Transcript cr.

    // Example usage
    [Book new set: { title: 'The Lord of the Rings'; author: 'J.R.R. Tolkien'; year: 1954; pages: 1207 }] printBookInfo.
```

**Explanation:**

- The code starts with the `class` keyword, which is used to define a new class. In this case, we are defining a class called `Book`.

- The `attributes:` line specifies the attributes that will be associated with each instance of the `Book` class. In this case, the attributes are `title`, `author`, `year`, and `pages`.

- The `class methods:` line specifies the class methods that are available for the `Book` class. In this case, we have defined a single class method called `new`, which is used to create new instances of the `Book` class.

- The `methods:` line specifies the instance methods that are available for instances of the `Book` class. In this case, we have defined four instance methods: `title`, `author`, `year`, and `pages`, which return the corresponding attribute values for the instance, and `printBookInfo`, which prints the book information to the Transcript.

- The `[Book new set: { title: 'The Lord of the Rings'; author: 'J.R.R. Tolkien'; year: 1954; pages: 1207 }] printBookInfo.` line creates a new instance of the `Book` class, sets the attribute values for the instance, and then calls the `printBookInfo` method on the instance. This will print the book information to the Transcript.