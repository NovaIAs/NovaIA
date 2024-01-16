```smalltalk
"Class hierarchy for a simple library system"

Object subclass: Library [
  "A library contains books, patrons, and circulation records."

  instanceVariableNames: 'books patrons records'

  classVariableNames: ''

  poolDictionaries: ''

  category: 'Library System'
].

Library class methods for: accessing [
  hasBooks [ :selector |
    "Answer whether there are any books in this library."

    ^ self books size > 0 ]

  hasPatrons [ :selector |
    "Answer whether there are any patrons in this library."

    ^ self patrons size > 0 ]

  hasRecords [ :selector |
    "Answer whether there are any circulation records in this library."

    ^ self records size > 0 ]
].

Library class methods for: finding [
  findBookByTitle [ :selector | title |
    "Answer the first book in this library with the given title, or nil if none."

    ^ self books detect: [ :book | book title = title ] ]

  findPatronByName [ :selector | name |
    "Answer the first patron in this library with the given name, or nil if none."

    ^ self patrons detect: [ :patron | patron name = name ] ]

  findRecordByBookAndPatron [ :selector | book patron |
    "Answer the circulation record for the given book and patron, or nil if none."

    ^ self records detect: [ :record | record book = book and: [ record patron = patron ] ] ]
].

Library methods for: adding [
  addBook [ :selector | book |
    "Add the given book to this library."

    self books add: book ]

  addPatron [ :selector | patron |
    "Add the given patron to this library."

    self patrons add: patron ]

  addRecord [ :selector | record |
    "Add the given circulation record to this library."

    self records add: record ]
].

Library methods for: removing [
  removeBook [ :selector | book |
    "Remove the given book from this library."

    self books remove: book ]

  removePatron [ :selector | patron |
    "Remove the given patron from this library."

    self patrons remove: patron ]

  removeRecord [ :selector | record |
    "Remove the given circulation record from this library."

    self records remove: record ]
].

Library methods for: printing [
  print [ :selector |
    "Print a summary of this library."

    Transcript show: 'Library: ' , self name ; cr.
    Transcript show: '  Books: ' , (self books size) asString ; cr.
    Transcript show: '  Patrons: ' , (self patrons size) asString ; cr.
    Transcript show: '  Records: ' , (self records size) asString ; cr.
    Transcript cr ]

  printBooks [ :selector |
    "Print a list of all books in this library."

    Transcript show: 'Books in ' , self name , ':' ; cr.
    self books do: [ :book | Transcript show: '  ' , book title ; cr ] ]

  printPatrons [ :selector |
    "Print a list of all patrons in this library."

    Transcript show: 'Patrons of ' , self name , ':' ; cr.
    self patrons do: [ :patron | Transcript show: '  ' , patron name ; cr ] ]

  printRecords [ :selector |
    "Print a list of all circulation records in this library."

    Transcript show: 'Records in ' , self name , ':' ; cr.
    self records do: [ :record | Transcript show: '  ' , record book title , ' checked out to ' , record patron name , ' on ' , record checkoutDate asString ; cr ] ]
].

Book subclass: Fiction [
  "A fiction book is a book that is not non-fiction."
].

Book subclass: NonFiction [
  "A non-fiction book is a book that is not fiction."
].

Book class methods for: creating [
  newFiction [ :selector | title author |
    "Answer a new fiction book with the given title and author."

    ^ self newFiction: title author: author ]

  newNonFiction [ :selector | title author |
    "Answer a new non-fiction book with the given title and author."

    ^ self newNonFiction: title author: author ]
].

Book methods for: printing [
  print [ :selector |
    "Print a description of this book."

    Transcript show: 'Book: ' , self title , ' by ' , self author , ' (' , self isbn , ')' ; cr ]
].

Patron subclass: Student [
  "A student is a patron who is also a student."
].

Patron subclass: Faculty [
  "A faculty member is a patron who is also a faculty member."
].

Patron class methods for: creating [
  newStudent [ :selector | name |
    "Answer a new student patron with the given name."

    ^ self newStudent: name ]

  newFaculty [ :selector | name |
    "Answer a new faculty patron with the given name."

    ^ self newFaculty: name ]
].

Patron methods for: printing [
  print [ :selector |
    "Print a description of this patron."

    Transcript show: 'Patron: ' , self name , ' (' , self patronID asString , ')' ; cr ]
].

CirculationRecord methods for: accessing [
  book [ :selector |
    "Answer the book associated with this circulation record."

    ^ self book ]

  patron [ :selector |
    "Answer the patron associated with this circulation record."

    ^ self patron ]

  checkoutDate [ :selector |
    "Answer the date this circulation record was created."

    ^ self checkoutDate ]
].

CirculationRecord methods for: printing [
  print [ :selector |
    "Print a description of this circulation record."

    Transcript show: 'Record: ' , self book title , ' checked out to ' , self patron name , ' on ' , self checkoutDate asString ; cr ]
].

"Example usage"

myLibrary := Library new.

myLibrary addBook: (Fiction newFiction: 'The Great Gatsby' author: 'F. Scott Fitzgerald').
myLibrary addBook: (NonFiction newNonFiction: 'A Brief History of Time' author: 'Stephen Hawking').

myLibrary addPatron: (Student newStudent: 'Jane Doe').
myLibrary addPatron: (Faculty newFaculty: 'John Smith').

myLibrary addRecord: (CirculationRecord new
  book: myLibrary findBookByTitle: 'The Great Gatsby'
  patron: myLibrary findPatronByName: 'Jane Doe'
  checkoutDate: Date today).

myLibrary addRecord: (CirculationRecord new
  book: myLibrary findBookByTitle: 'A Brief History of Time'
  patron: myLibrary findPatronByName: 'John Smith'
  checkoutDate: Date today).

myLibrary print.
myLibrary printBooks.
myLibrary printPatrons.
myLibrary printRecords.
```

This code defines a simple library system in Smalltalk. It includes classes for libraries, books, patrons, and circulation records. The library class has methods for adding and removing books, patrons, and records, as well as methods for printing a summary of the library. The book class has methods for printing a description of the book. The patron class has methods for printing a description of the patron. The circulation record class has methods for accessing the book, patron, and checkout date, as well as a method for printing a description of the record.

The example usage at the end of the code creates a new library, adds some books and patrons to the library, and adds some circulation records to the library. It then prints a summary of the library, a list of all books in the library, a list of all patrons in the library, and a list of all circulation records in the library.