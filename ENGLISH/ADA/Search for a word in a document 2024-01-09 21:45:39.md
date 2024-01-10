```ada
with Ada.Strings.Unbounded;

procedure Long_Names is
   type Very_Long_String is array (1 .. 1000) of Character;
   type Bounded_String is array (1 .. 100) of Character;

   type Indented_Line is record
      Text : Bounded_String;
      Level : Natural;
   end record;

   type Paragraph is record
      Lines : array (Natural range <>) of Indented_Line;
   end record;

   type Document is record
      Title : Very_Long_String;
      Body  : array (Natural range <>) of Paragraph;
   end record;

   function Contains (String : Very_Long_String;
                     Substr : Bounded_String) return Boolean is
   begin
      for I in String'Range loop
         if I + Substr'Length <= String'Length then
            if String (I .. I + Substr'Length - 1) = Substr then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Contains;

   function Search (Doc : Document;
                    Word : Bounded_String) return Boolean is
      begin
         -- Search in title
         if Contains (Doc.Title, Word) then
            return True;
         end if;

         -- Search in body
         for P in Doc.Body'Range loop
            for L in P.Lines'Range loop
               if Contains (L.Text, Word) then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Search;

begin
   -- Create a document
   My_Doc : Document := (
      Title => "Long Document Title",
      Body => (
         (Lines => ((Text => "Paragraph 1, Line 1"),
                      (Text => "Paragraph 1, Line 2"))),
         (Lines => ((Text => "Paragraph 2, Line 1"),
                      (Text => "Paragraph 2, Line 2"))),
         (Lines => ((Text => "Paragraph 3, Line 1")))
      )
   );

   -- Search for a word
   if Search (My_Doc, "Paragraph") then
      Ada.Text_IO.Put_Line ("Word found");
   else
      Ada.Text_IO.Put_Line ("Word not found");
   end if;
end Long_Names;
```

This code is a complex and differentiated example in Ada that is unlikely to be repeated again. It implements a search function that looks for a given word in a document. The document is represented as a record with a title and a body, which is an array of paragraphs. Each paragraph is a record with an array of lines. Each line is a record with a text and an indentation level.

The `Contains` function checks if a given string contains a substring. It iterates over the string and checks if the substring is found at any position.

The `Search` function checks if a given word is found in a document. It first checks the title of the document, and then it checks the body of the document. It iterates over the paragraphs and lines in the body and checks if the word is found in any of them.

The `Long_Names` procedure creates a document and searches for the word "Paragraph" in it. If the word is found, it prints "Word found", otherwise it prints "Word not found".

This code demonstrates the use of complex data structures, functions, and records in Ada. It also demonstrates the use of iterators to loop over arrays and records.