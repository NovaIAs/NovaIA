```uml
@startuml
skinparam linetype ortho
skinparam roundcorner 15
skinparam shadowing false

title UML Class Diagram of a Library System

package org.example.library {

  abstract class Person {
    private String name;
    private String address;
    private String phoneNumber;

    + Person(String name, String address, String phoneNumber)
    + String getName()
    + void setName(String name)
    + String getAddress()
    + void setAddress(String address)
    + String getPhoneNumber()
    + void setPhoneNumber(String phoneNumber)
  }

  class Member extends Person {
    private String membershipNumber;
    private Date membershipStartDate;
    private Date membershipEndDate;

    + Member(String name, String address, String phoneNumber, String membershipNumber, Date membershipStartDate, Date membershipEndDate)
    + String getMembershipNumber()
    + void setMembershipNumber(String membershipNumber)
    + Date getMembershipStartDate()
    + void setMembershipStartDate(Date membershipStartDate)
    + Date getMembershipEndDate()
    + void setMembershipEndDate(Date membershipEndDate)
  }

  class Librarian extends Person {
    private String employeeID;
    private Date hireDate;

    + Librarian(String name, String address, String phoneNumber, String employeeID, Date hireDate)
    + String getEmployeeID()
    + void setEmployeeID(String employeeID)
    + Date getHireDate()
    + void setHireDate(Date hireDate)
  }

  class Book {
    private String ISBN;
    private String title;
    private String author;
    private String publisher;
    private Date publicationDate;
    private int numberOfPages;

    + Book(String ISBN, String title, String author, String publisher, Date publicationDate, int numberOfPages)
    + String getISBN()
    + void setISBN(String ISBN)
    + String getTitle()
    + void setTitle(String title)
    + String getAuthor()
    + void setAuthor(String author)
    + String getPublisher()
    + void setPublisher(String publisher)
    + Date getPublicationDate()
    + void setPublicationDate(Date publicationDate)
    + int getNumberOfPages()
    + void setNumberOfPages(int numberOfPages)
  }

  class Loan {
    private Date loanDate;
    private Date dueDate;
    private Date returnDate;
    private Member member;
    private Book book;

    + Loan(Date loanDate, Date dueDate, Date returnDate, Member member, Book book)
    + Date getLoanDate()
    + void setLoanDate(Date loanDate)
    + Date getDueDate()
    + void setDueDate(Date dueDate)
    + Date getReturnDate()
    + void setReturnDate(Date returnDate)
    + Member getMember()
    + void setMember(Member member)
    + Book getBook()
    + void setBook(Book book)
  }

  class Library {
    private String name;
    private String address;
    private String phoneNumber;
    private List<Member> members;
    private List<Librarian> librarians;
    private List<Book> books;
    private List<Loan> loans;

    + Library(String name, String address, String phoneNumber)
    + String getName()
    + void setName(String name)
    + String getAddress()
    + void setAddress(String address)
    + String getPhoneNumber()
    + void setPhoneNumber(String phoneNumber)
    + List<Member> getMembers()
    + void setMembers(List<Member> members)
    + List<Librarian> getLibrarians()
    + void setLibrarians(List<Librarian> librarians)
    + List<Book> getBooks()
    + void setBooks(List<Book> books)
    + List<Loan> getLoans()
    + void setLoans(List<Loan> loans)
  }
}
@enduml
```

Explanation:

- The code above is a Unified Modeling Language (UML) class diagram of a library system.
- The diagram consists of several classes, including Person, Member, Librarian, Book, Loan, and Library.
- The Person class is an abstract class that represents a person. It has three attributes: name, address, and phoneNumber.
- The Member class extends the Person class and represents a member of the library. It has additional attributes such as membershipNumber, membershipStartDate, and membershipEndDate.
- The Librarian class also extends the Person class and represents a librarian. It has an additional attribute employeeID and hireDate.
- The Book class represents a book in the library. It has attributes such as ISBN, title, author, publisher, publicationDate, and numberOfPages.
- The Loan class represents a loan of a book from the library. It has attributes such as loanDate, dueDate, returnDate, member, and book.
- The Library class represents the library itself. It has attributes such as name, address, phoneNumber, members, librarians, books, and loans.

- The relationships between the classes are represented by lines and arrows.
- A solid line with an arrowhead represents a one-to-many relationship. For example, a single library can have many members.
- A dashed line with an arrowhead represents a zero-to-many relationship. For example, a single book can be loaned to many members or not at all.
- The diagram also includes some annotations, which are textual notes that provide additional information about the classes and relationships.