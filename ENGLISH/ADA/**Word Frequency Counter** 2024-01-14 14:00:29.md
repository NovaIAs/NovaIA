```ada
with Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Integer_Text_IO;

-- Define a fixed-length string type to hold a single word.
type Word is array (1..20) of Character;

-- Define a record type to hold a word and its frequency.
type Word_Count is record
    word : Word;
    count : Integer;
end record;

-- Define an unbounded string type to hold the input text.
type Text is array (Positive range <>) of Character;

-- Define a procedure to read the input text from the standard input.
procedure Get_Text (Text : out Text) is
    Line : Unbounded_String;
begin
    Text := "";
    while not Ada.Text_IO.End_Of_File loop
        Ada.Text_IO.Get_Line (Line, Ada.Text_IO.Standard_Input);
        Text := Text & Line & "\n";
    end loop;
end Get_Text;

-- Define a function to convert a string to lowercase.
function To_Lower (S : Unbounded_String) return Unbounded_String is
begin
    S := S & Ada.Strings.Fixed.Null_Character;
    for I in 1..S'Length loop
        S(I) := Ada.Strings.Fixed.To_Lower (S(I));
    end loop;
    return S(1..S'Length-1);
end To_Lower;

-- Define a procedure to split the input text into words.
procedure Split_Text (Text : in Text; Words : out array of Word) is
    Word_Start : Positive;
    Word_End : Positive;
    I : Positive;
begin
    Word_Start := 1;
    Word_End := 1;
    for I in 1..Text'Length loop
        if Text(I) in Ada.Strings.Fixed.Space then
            Words(Word_Start) := To_Lower (Text(Word_Start..Word_End-1));
            Word_Start := I + 1;
            Word_End := I + 1;
        else
            Word_End := Word_End + 1;
        end if;
    end loop;
    Words(Word_Start) := To_Lower (Text(Word_Start..Text'Length));
end Split_Text;

-- Define a procedure to count the frequency of each word in the input text.
procedure Count_Words (Words : in array of Word; Counts : out array of Word_Count) is
    Word_Map : Ada.Strings.Fixed.Hash := Ada.Strings.Fixed.Create_Hash;
    I : Positive;
begin
    for I in 1..Words'Length loop
        if Ada.Strings.Fixed.Find (Word_Map, Words(I)) then
            Counts(Ada.Strings.Fixed.Index (Word_Map, Words(I))).count :=
                Counts(Ada.Strings.Fixed.Index (Word_Map, Words(I))).count + 1;
        else
            Ada.Strings.Fixed.Insert (Word_Map, Words(I), I);
            Counts(I).word := Words(I);
            Counts(I).count := 1;
        end if;
    end loop;
end Count_Words;

-- Define a procedure to sort the word counts in descending order of frequency.
procedure Sort_Counts (Counts : in out array of Word_Count) is
    Swap : Word_Count;
    I : Positive;
    J : Positive;
begin
    for I in 1..Counts'Length-1 loop
        for J in I+1..Counts'Length loop
            if Counts(I).count < Counts(J).count then
                Swap := Counts(I);
                Counts(I) := Counts(J);
                Counts(J) := Swap;
            end if;
        end loop;
    end loop;
end Sort_Counts;

-- Define a procedure to print the word counts to the standard output.
procedure Print_Counts (Counts : in array of Word_Count) is
begin
    for I in 1..Counts'Length loop
        Ada.Text_IO.Put_Line (Counts(I).word & " : " & Integer'Image (Counts(I).count));
    end loop;
end Print_Counts;

-- Main program.
with Ada.Command_Line;

procedure Word_Frequency is
    Text : Text;
    Words : array (1..1000) of Word;
    Counts : array (1..1000) of Word_Count;
begin
    if Ada.Command_Line.Argument_Count < 2 then
        Ada.Text_IO.Put_Line ("Usage: word_frequency <input_file>");
        Ada.Text_IO.Put_Line ("Reads a text file and prints the frequency of each word in the file.");
        return;
    end if;
    Get_Text (Text);
    Split_Text (Text, Words);
    Count_Words (Words, Counts);
    Sort_Counts (Counts);
    Print_Counts (Counts);
end Word_Frequency;
```

**Explanation:**

This Ada program reads a text file, splits the text into words, counts the frequency of each word, sorts the word counts in descending order of frequency, and prints the word counts to the standard output.

The program first defines a number of types and procedures that are used in the program. The `Word` type is a fixed-length string type that can hold a single word. The `Word_Count` type is a record type that holds a word and its frequency. The `Text` type is an unbounded string type that can hold the input text.

The `Get_Text` procedure reads the input text from the standard input and stores it in the `Text` variable.

The `To_Lower` function converts a string to lowercase.

The `Split_Text` procedure splits the input text into words. The procedure takes the input text and an array of words as arguments. The procedure iterates through the input text and splits it into words based on spaces. The words are stored in the array of words.

The `Count_Words` procedure counts the frequency of each word in the input text. The procedure takes the array of words and an array of word counts as arguments. The procedure iterates through the array of words and increments the count for each word in the array of word counts.

The `Sort_Counts` procedure sorts the word counts in descending order of frequency. The procedure takes the array of word counts as an argument. The procedure uses the bubble sort algorithm to sort the array of word counts.

The `Print_Counts` procedure prints the word counts to the standard output. The procedure takes the array of word counts as an argument. The procedure iterates through the array of word counts and prints each word and its frequency to the standard output.

The `Word_Frequency` procedure is the main program. The procedure first checks if the user has provided a command-line argument. If the user has not provided a command-line argument, the procedure prints a usage message and returns. Otherwise, the procedure calls the `Get_Text` procedure to read the input text from the file specified by the command-line argument. The procedure then calls the `Split_Text` procedure to split the input text into words. The procedure then calls the `Count_Words` procedure to count the frequency of each word in the input text. The procedure then calls the `Sort_Counts` procedure to sort the word counts in descending order of frequency. Finally, the procedure calls the `Print_Counts` procedure to print the word counts to the standard output.