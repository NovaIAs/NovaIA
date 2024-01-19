**Task:** Design and implement a program in Ada to manage a library inventory system. The program should allow the user to add, delete, search, and update records of books, authors, and publishers. Additionally, the program should generate reports on books, authors, and publishers.

**Code:**

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Library_Inventory is
   type Book_ID is new Positive;
   type Author_ID is new Positive;
   type Publisher_ID is new Positive;

   type Book is record
      ID : Book_ID;
      Title : String(1..20);
      Author : Author_ID;
      Publisher : Publisher_ID;
      Year : Positive;
      Pages : Positive;
   );

   type Author is record
      ID : Author_ID;
      Name : String(1..20);
      Nationality : String(1..20);
      Date_of_Birth : Date;
   );

   type Publisher is record
      ID : Publisher_ID;
      Name : String(1..20);
      Address : String(1..50);
      Phone : String(1..20);
   );

   Books : Ada.Strings.Maps.Hash_Map(Book_ID, Book);
   Authors : Ada.Strings.Maps.Hash_Map(Author_ID, Author);
   Publishers : Ada.Strings.Maps.Hash_Map(Publisher_ID, Publisher);

   procedure Add_Book(Book : in Book) is
      Books(Book.ID) := Book;
   end Add_Book;

   procedure Delete_Book(Book_ID : in Book_ID) is
      Books.Delete(Book_ID);
   end Delete_Book;

   procedure Search_Book(Book_ID : in Book_ID) return Book is
   begin
      return Books(Book_ID);
   end Search_Book;

   procedure Update_Book(Book : in Book) is
      Books(Book.ID) := Book;
   end Update_Book;

   procedure Add_Author(Author : in Author) is
      Authors(Author.ID) := Author;
   end Add_Author;

   procedure Delete_Author(Author_ID : in Author_ID) is
      Authors.Delete(Author_ID);
   end Delete_Author;

   procedure Search_Author(Author_ID : in Author_ID) return Author is
   begin
      return Authors(Author_ID);
   end Search_Author;

   procedure Update_Author(Author : in Author) is
      Authors(Author.ID) := Author;
   end Update_Author;

   procedure Add_Publisher(Publisher : in Publisher) is
      Publishers(Publisher.ID) := Publisher;
   end Add_Publisher;

   procedure Delete_Publisher(Publisher_ID : in Publisher_ID) is
      Publishers.Delete(Publisher_ID);
   end Delete_Publisher;

   procedure Search_Publisher(Publisher_ID : in Publisher_ID) return Publisher is
   begin
      return Publishers(Publisher_ID);
   end Search_Publisher;

   procedure Update_Publisher(Publisher : in Publisher) is
      Publishers(Publisher.ID) := Publisher;
   end Update_Publisher;

   procedure Generate_Book_Report is
      for Book in Books.Values loop
         Ada.Text_IO.Put_Line(Book.Title);
      end loop;
   end Generate_Book_Report;

   procedure Generate_Author_Report is
      for Author in Authors.Values loop
         Ada.Text_IO.Put_Line(Author.Name);
      end loop;
   end Generate_Author_Report;

   procedure Generate_Publisher_Report is
      for Publisher in Publishers.Values loop
         Ada.Text_IO.Put_Line(Publisher.Name);
      end loop;
   end Generate_Publisher_Report;

begin
   -- Add some sample data
   Add_Book(Book(1, "The Lord of the Rings", 2, 3, 1954, 1207));
   Add_Author(Author(1, "J.R.R. Tolkien", "British", Date'1892-01-03'));
   Add_Publisher(Publisher(1, "George Allen & Unwin", "London, UK", "+44 (0)20 7405 0801"));

   -- Search for a book
   Ada.Text_IO.Put_Line("Book Title:");
   Book_Title : String(1..20);
   Ada.Text_IO.Get_Line(Book_Title, Ada.Text_IO.New_Line);
   Book_ID := Books.Find_Index(Book_Title);
   if Book_ID /= 0 then
      Ada.Text_IO.Put_Line("Book Found:");
      Ada.Text_IO.Put_Line(Search_Book(Book_ID).Title);
   else
      Ada.Text_IO.Put_Line("Book Not Found");
   end if;

   -- Generate reports
   Generate_Book_Report;
   Generate_Author_Report;
   Generate_Publisher_Report;
end Library_Inventory;
```

**Explanation:**

1. We define custom types for Book, Author, and Publisher, which represent the entities in our library inventory system.
2. We create hash maps to store the books, authors, and publishers, using their IDs as keys and the corresponding records as values.
3. We define procedures to perform CRUD (Create, Read, Update, Delete) operations on the books, authors, and publishers.
4. We define procedures to generate reports on the books, authors, and publishers.
5. In the main program, we add some sample data to the library inventory system.
6. We search for a book based on the user's input.
7. We generate reports on the books, authors, and publishers.