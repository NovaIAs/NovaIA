```ada
with Ada.Text_IO;

function Is_Leap_Year (Year : in Integer) return Boolean is
begin
   return ((Year mod 4 = 0 and not Year mod 100 = 0) or Year mod 400 = 0);
end Is_Leap_Year;

procedure Print_Calendar (Year : in Integer) is
   Calendar : constant String (1..24) := "|      January        |   February    |   March  " &
                                               "|   April        |     May       |   June   " &
                                               "|    July        |   August   |   September" &
                                               "|   October      | November  |   December ";
begin
   Put_Line (Calendar);
   for Month in 1..12 loop
      Put_Line ("|--------------------|--------------------|-------------------- ");
      Put ("| ");
      Justify (Month'Image (Width => 3), Center, ' ');
      Put (" ");

      if Is_Leap_Year (Year) and Month = 2 then
         for Day in 1..29 loop
            Justify (Day'Image (Width => 2), Center, ' ');
            Put (" ");
            if Day >= 28 and Day mod 7 = 0 then
               Put_Line ("| ");
            end if;
         end loop;
      else
         for Day in 1..30 + (Month in (4, 6, 9, 11)) loop
            Justify (Day'Image (Width => 2), Center, ' ');
            Put (" ");
            if Day >= 28 and Day mod 7 = 0 then
               Put_Line ("| ");
            end if;
         end loop;
      end if;
   end loop;
end Print_Calendar;

begin
   Print_Calendar (2000);
end Ada_Calendar.
```

This program prints a yearly calendar to the console.

The `Is_Leap_Year` function checks if a given year is a leap year.

The `Print_Calendar` procedure takes a year as an argument and prints a calendar for that year. The calendar is printed in a 3x4 grid, with the months of the year printed in the first column. The days of the week are printed in the first row, and the days of the month are printed in the remaining rows.

The `Justify` procedure is used to center the text in the calendar.

The `Put` and `Put_Line` procedures are used to output text to the console.

The `Month` loop is used to iterate through the months of the year.

The `Day` loop is used to iterate through the days of each month.

The `if` statement checks if the month is February and the year is a leap year. If so, the loop iterates through 29 days instead of 28.

The `if` statement inside the `Day` loop checks if the day is greater than or equal to 28 and if the day is divisible by 7. If so, a newline is printed to the console to start a new row in the calendar.