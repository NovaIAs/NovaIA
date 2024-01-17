```ada
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

procedure Clone_File is
   File_Name : String(1..80);
   Source : File_Type;
   Destination : File_Type;
   Buffer : array(1..1024) of Character;
   Bytes_Read : Positive;
begin
   Ada.Text_IO.Put_Line("Enter the source file name: ");
   Ada.Text_IO.Get_Line(File_Name);
   Ada.Streams.Stream_IO.Open(Source, Ada.Streams.File_Mode.In,
      File_Name);
   Ada.Text_IO.Put_Line("Enter the destination file name: ");
   Ada.Text_IO.Get_Line(File_Name);
   Ada.Streams.Stream_IO.Open(Destination, Ada.Streams.File_Mode.Out,
      File_Name);
   loop
      Bytes_Read := Ada.Streams.Stream_IO.Read(Source, Buffer,
         Buffer'Range(1..Buffer'Last));
      if Bytes_Read = 0 then
         exit;
      end if;
      Ada.Streams.Stream_IO.Write(Destination, Buffer(1..Bytes_Read));
   end loop;
   Ada.Streams.Stream_IO.Close(Source);
   Ada.Streams.Stream_IO.Close(Destination);
   Ada.Text_IO.Put_Line("File cloned successfully.");
end Clone_File;
```

Explanation:

* The code starts by including the necessary Ada libraries for input and output operations.
* The `Clone_File` procedure is defined, which takes no parameters and returns nothing.
* Inside the procedure, several variables are declared:
    * `File_Name` is a string variable that will store the names of the source and destination files.
    * `Source` and `Destination` are file type variables that will represent the source and destination files, respectively.
    * `Buffer` is an array of characters that will be used to read and write data from and to the files.
    * `Bytes_Read` is a positive integer variable that will store the number of bytes read from the source file.
* The user is prompted to enter the source and destination file names using the `Ada.Text_IO.Put_Line` and `Ada.Text_IO.Get_Line` procedures.
* The source file is opened in read mode using the `Ada.Streams.Stream_IO.Open` procedure.
* The destination file is opened in write mode using the `Ada.Streams.Stream_IO.Open` procedure.
* A loop is used to read data from the source file, 1024 bytes at a time, using the `Ada.Streams.Stream_IO.Read` procedure.
* If the number of bytes read is 0, it means that the end of the file has been reached, so the loop is exited.
* The data read from the source file is written to the destination file using the `Ada.Streams.Stream_IO.Write` procedure.
* Once all the data has been copied from the source to the destination file, the source and destination files are closed using the `Ada.Streams.Stream_IO.Close` procedure.
* Finally, a message is displayed to the user indicating that the file was cloned successfully.