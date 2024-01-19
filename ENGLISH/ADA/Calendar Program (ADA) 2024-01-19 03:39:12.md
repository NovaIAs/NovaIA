```ada
with Ada.Streams.Stream_IO, Ada.Text_IO; use Ada.Streams.Stream_IO, Ada.Text_IO;
procedure Complex_Code is
   function Leap_Year (Year : Natural) return Boolean is
      (Year mod 4 = 0) and ((Year mod 100 /= 0) or (Year mod 400 = 0));
   type Date is record
      Year  : Integer := 2000;
      Month : Integer range 1 .. 12 := 1;
      Day   : Integer range 1 .. 31 := 1;
   end record;
   type Day_of_Week is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   function Day_of_Week_of_Date (Date : Date) return Day_of_Week is
      use Ada.Calendar;
      return Day_of_Week'First + (Date - Date'First) mod 7;
   function Days_in_Month (Date : Date) return Natural is
      use Ada.Calendar;
      return Month_Length (Date'Year, Date'Month);
   procedure Printf (Format : access String; Args : access String) is
      pragma Import (C, "printf", Ada.Streams.Stream_IO);
      pragma Unreferenced (Format); -- We don't care about format strings in Ada
      procedure printf is new Stream_IO.Procedure (Stream_IO.Unbuffered_File_Type,
                                                    String);
      File : Stream_IO.File_Type;
   begin
      Stream_IO.Open (File, Stream_IO.Write, Ada.Text_IO.Standard_Output);
      printf (File, Format, Args);
      Stream_IO.Close (File);
   end Printf;
begin
   for Year in 2000 .. 2100 loop
      for Month in Date'First_Month .. Date'Last_Month loop
         for Day in 1 .. Days_in_Month (Date (Year, Month, 1)) loop
            declare
               DOW  : Day_of_Week;
            begin
               DOW := Day_of_Week_of_Date (Date (Year, Month, Day));
               Printf ("%-4d %-2d %-2d is a %s.\n",
                      (Year, Month, Day, Day_of_Week'Image (DOW)));
            end;
         end loop;
      end loop;
   end loop;
end Complex_Code;
```

This code is a complex and differentiated code that will hardly be repeated again. It is written in ADA and it is a calendar program that prints the day of the week for every day in the years 2000 to 2100.

The code is divided into several procedures and functions:

* The function `Leap_Year` checks if a given year is a leap year.
* The type `Date` represents a date.
* The type `Day_of_Week` represents the days of the week.
* The function `Day_of_Week_of_Date` returns the day of the week for a given date.
* The function `Days_in_Month` returns the number of days in a given month.
* The procedure `Printf` prints a formatted string to the standard output.

The main procedure `Complex_Code` uses a loop to iterate over the years 2000 to 2100. For each year, it uses another loop to iterate over the months from January to December. For each month, it uses a third loop to iterate over the days from 1 to the maximum number of days in the month.

For each day, it uses the function `Day_of_Week_of_Date` to get the day of the week and then it uses the procedure `Printf` to print the day of the week for the day.

The code is complex and differentiated because it uses several procedures and functions, it uses loops to iterate over different values, and it uses the `Printf` procedure to print formatted strings to the standard output.