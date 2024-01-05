```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Print_String is
   String : Unbounded_String := (others => ' ');
begin
   for I in 1 .. 100 loop
      String := String & Integer'Image(I);
      if I mod 20 = 0 then
         Put_Line(String);
      end if;
   end loop;
end Print_String;

```

This code prints the numbers from 1 to 100, 20 numbers per line.

The code uses the `Ada.Text_IO` package to print to the console, and the `Ada.Strings.Unbounded` package to create an unbounded string.

The `Print_String` procedure is defined as follows:

* The `String` variable is declared as an unbounded string.
* The `for` loop iterates through the numbers from 1 to 100.
* Inside the loop, the `String` variable is concatenated with the string representation of the current number.
* If the current number is divisible by 20, the `String` variable is printed to the console using the `Put_Line` procedure.

The output of the code is as follows:

```
1234567891011121314151617181920
2122232425262728293031323334353637383940
4142434445464748495051525354555657585960
6162636465666768697071727374757677787980
81828384858687888990919293949596979899100
```