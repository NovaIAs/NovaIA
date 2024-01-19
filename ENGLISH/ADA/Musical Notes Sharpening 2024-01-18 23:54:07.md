```
with Ada.Text_IO, Ada.Strings.Fixed;  use Ada.Text_IO, Ada.Strings.Fixed;

procedure Musical_Notes is
   type Note is new String(1..3);

   procedure Play (Note : in Note) is
   begin
      Put(Note);
      New_Line;
   end Play;

private
   function Is_Sharp (Note : Note) return Boolean is
   begin
      return Note(3) = '#';
   end Is_Sharp;

   procedure Sharpen (Note : in out Note) is
   begin
      if not Is_Sharp(Note) then
         Note(3) := '#';
      end if;
   end Sharpen;

   function Sharpened (Note : Note) return Note is
   begin
      declare
         Temporary_Note : Note := Note;
      begin
         Sharpen(Temporary_Note);
         return Temporary_Note;
      end;
   end Sharpened;

   procedure Play_Sharp (Note : Note) is
   begin
      Play(Sharpened(Note));
   end Play_Sharp;

   Scale : constant array (1..7) of Note := ("C", "D", "E", "F", "G", "A", "B");
begin
   for I in Scale'range loop
      Put(Scale(I));
      New_Line;
   end loop;

   for I in Scale'range loop
      Play_Sharp(Scale(I));
   end loop;
end Musical_Notes;
```

This code defines a type called **Note** that is a string with a maximum length of 3 characters. It then declares a procedure called **Play** that takes a **Note** as an argument and prints it out to the console. The code also defines a function called **Is_Sharp** that takes a **Note** as an argument and returns a Boolean value indicating whether the note is sharp. A procedure called **Sharpen** is defined that takes a **Note** as an argument and sharps it if it is not already sharp. A function called **Sharpened** is also defined that returns a new **Note** that is the sharped version of the original **Note**. Finally, a procedure called **Play_Sharp** is defined that takes a **Note** as an argument and plays the sharped version of the note.

The main part of the code declares an array of **Note**s called **Scale** and initializes it with the values of the notes in the C major scale. The code then uses a **for** loop to iterate through the **Scale** array and print out each note to the console. Another **for** loop is used to iterate through the **Scale** array and play each note using the **Play_Sharp** procedure.