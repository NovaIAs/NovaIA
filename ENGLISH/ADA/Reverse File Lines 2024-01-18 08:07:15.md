```ada
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_Strings is
   type File_Type is access Ada.Strings.Unbounded.Utf_String;
   function Open (File_Name : in String) return File_Type is
   begin
      return new Ada.Strings.Unbounded.Utf_String (File_Name);
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      File.Close;
   end Close;

   function Read_Line (File : in File_Type) return String is
   begin
      declare
         Line : String (1 .. 80);
      begin
         File.Get_Line (Line);
         return Line;
      end;
   end Read_Line;

   procedure Echo_Lines (File : in File_Type) is
   begin
      loop
         Put_Line (Read_Line (File));
      end loop;
   end Echo_Lines;

   procedure Process_Line (File : in File_Type; Line : in String) is
   begin
      declare
         Result  : String (1 .. 80);
         Natural : Integer;
      begin
         Natural := Line'Last + 1;
         while (Natural /= 0) loop
            Result (Natural) := Line (Natural);
            Natural := Natural - 1;
         end loop;
         Put_Line (Result);
      end;
   end Process_Line;

   procedure Process_File (File_Name : in String) is
   begin
      declare
         File : File_Type;
      begin
         File := Open (File_Name);
         loop
            declare
               Line : String (1 .. 80);
            begin
               Line := Read_Line (File);
               Process_Line (File, Line);
            end;
         exit when File.Is_Last;
         end loop;
      exception
         when Ada.IO_Exceptions.Status_Error =>
            Ada.Text_IO.Put ("Error reading file " & File_Name);
      end;
   end Process_File;

begin
   Process_File ("in.txt");
end Find_Strings;
```

This code reads a text file, reverses each line, and prints the reversed lines to the standard output. The code is written in Ada, a high-level programming language known for its reliability and safety.

The `Open` and `Close` procedures are used to open and close the text file. The `Read_Line` function reads a line of text from the file. The `Echo_Lines` procedure prints the lines of text to the standard output. The `Process_Line` function reverses a line of text. The `Process_File` procedure opens the file, processes each line of text, and closes the file.

The main program `Find_Strings` calls the `Process_File` procedure to process the file "in.txt". The `Process_File` procedure opens the file, processes each line of text, and closes the file. The `Process_Line` function reverses each line of text. The `Echo_Lines` procedure prints the reversed lines of text to the standard output.