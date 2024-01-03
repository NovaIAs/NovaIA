```ada
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure Transform_Text is

   package Ada.Characters.Handling renames Ada.Strings.Fixed;
   use Ada.Characters.Handling;

   package Fixed_Strings renames Ada.Strings.Fixed;
   use Fixed_Strings;

   -- Uncomment the following if >= Ada 2012 is available:
   -- package Unbounded_Strings renames Ada.Strings.Unbounded;
   -- use Unbounded_Strings;

   -- Uncomment one of the following:
   -- type String is String(1..80);
   -- type String is Bounded_String(1..80);
   -- type String is Unbounded_String;

   function Length(Item : String) return Natural;
   function Index(Item : String; Substring : String) return Natural;
   function Reverse(Item : String) return String;

begin

   declare
      A, B, C, R : String;
   begin
      C := "   THIS IS A TEST   ";
      -- Trim the leading and trailing spaces from string C
      C := Trim(C);

      -- Convert string C to all lowercase
      C := To_Lower(C);

      -- Insert a comma after the third character in string C
      A := Insert("abc,def", ' ', Index(C, "A") + 3);

      -- Right-justify the string C in a field of 20 characters
      B := Right_Justify(C, 20);

      -- Construct a new string consisting of string A followed by string B
      R := A & B;

      -- Display the new string
      Put_Line(R);
   end;

end Transform_Text;
```

This code performs various text manipulation operations. Here's a detailed explanation:

1. **Package Renaming**:
   - **Ada.Characters.Handling** is renamed to **Ada.Characters.Handling**.
   - **Ada.Strings.Fixed** is renamed to **Fixed_Strings**.

   This is done to avoid conflicts with other packages and to reduce the length of the package names.

2. **Type Declarations**:
   - **String** is defined as a bounded string of characters with a maximum length of 80. You can uncomment one of the given options to select the desired string type.

3. **Function Declarations**:
   - **Length**: Returns the length of the given string.
   - **Index**: Returns the index of the first occurrence of the substring in the given string.
   - **Reverse**: Reverses the characters in a given string.

4. **Main Program**:
   - **declare** section: Declares local variables **A**, **B**, **C**, and **R** of type **String**.
   - **begin** section:
     - **C** is initialized to a string with leading and trailing spaces.
     - **C** is trimmed to remove the leading and trailing spaces.
     - **C** is converted to lowercase using **To_Lower**.
     - **A** is created by inserting a comma after the third character in **C**.
     - **B** is created by right-justifying **C** in a field of 20 characters.
     - **R** is constructed by concatenating **A** and **B**.
     - **R** is displayed on the console using **Put_Line**.

In summary, this code demonstrates various text manipulation techniques, including trimming, converting to lowercase, inserting characters, right-justifying, concatenation, and displaying the result.