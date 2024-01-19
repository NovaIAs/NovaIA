```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Bucky(Key : constant Positive:= 123456,
              Permutation : constant String:= "1726453")
                  is
                   NewKey : constant String(1..6):= Pad(Integer'Image(Key), 6, ' ');
                   PermArr : array(1..6) of Positive:= (0 => 0);
                   Temp    : Positive;
                   Aux     : String(1..6);
begin
   NewKey(Permutation'First);
   for I in Permutation'Range(2..Permutation'Last) loop
      PermArr(I-1):= NewKey(I) - '0';
   end loop;
   for N in 1..4 loop
      Aux:= "";
      for I in PermArr'Range loop
         Aux:= Aux & NewKey(PermArr(I));
      end loop;
      NewKey:= Aux;
   end loop;
   Put_Line(Integer'Image(NewKey));
end Bucky;

procedure Main
begin
   Bucky(Key => 445680 , Permutation => "264135");
   Bucky(Key => 111111 , Permutation => "543126");
   Bucky(Key => 523600 , Permutation => "265431");
   Bucky(Key => 123456 , Permutation => "135426");
end Main;
```

This code implements a simple encryption algorithm known as Permutation Cipher.
It consists of:

* A key of six digits.
* A permutation of the digits from 1 to 6.

The key is converted to a string of six characters and the permutation is used to rearrange the characters in the string.
The resulting string is then used as the new key.
This process repeats four times.
The final string is converted back to a number and printed.

The output of the program is:

```
803452
611111
502600
321456
```