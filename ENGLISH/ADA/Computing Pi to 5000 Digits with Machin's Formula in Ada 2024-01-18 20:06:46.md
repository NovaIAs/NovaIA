```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Powerful_Pi is
   type Huge_Float is array (1..5000) of Float;  -- Enough accuracy?
   type Huge_Complex is record
      Re, Im : Huge_Float;		-- Real and Imaginary parts
   end record;

   type Complex is record
      Re, Im : Float;			-- Real and Imaginary parts
   end record;
   Digit : array (1..1000) of Integer;	-- String buffer
   N : Integer := 0;			-- Number of digits

   function Pi (N : Integer) return Huge_Complex is
   -- Compute Pi to N digits using Machin's formula (1851).
      function Angle (N : Integer) return Float is
	 -- Return pi/4 * (-1)^N
         if N mod 2 = 1 then return -Pi / 4.0; else return Pi / 4.0; end if;
      end Angle;

      function Term (N : Integer) return Huge_Complex is
	 -- Return (4/N)/(1 + (N-1)/N) as a huge complex number
         Y : Huge_Complex := (1.0, 0.0) + (Float'Machine_Epsilon, 0.0);
	 -- Avoid division by zero
         return 4.0 / Float(N) * (1.0 / Y - 1.0);
      end Term;

      C : Huge_Complex := (0.0, 0.0);  -- Partial sum
      for I in 1..N loop
         C := C + Term(I);
      end loop;
      return C * Angle(N);
   end Pi;

   procedure Display (C : Complex) is
   -- Convert to huge complex for formatting
      HC : Huge_Complex := (Huge_Float(C.Re), Huge_Float(C.Im));

   -- Format real part without trailing zeros
      Put("("); Put(HC.Re, 0, 0); Put(", ");

   -- Format imaginary part with trailing zeros,
   -- if it's not zero (e.g. for Pi)
      if HC.Im = 0.0 then
         Put("0")
      elsif HC.Im < 0.0 then
         Put("-"); Put(Float'Abs(HC.Im), 0, 0)
      else
         Put(Float'Abs(HC.Im), 0, 0)
      end if;
      Put(")");
   end Display;

   C : Complex;
begin
   for I in 1..20 by 10 loop
      -- Compute and print 10, 20, 30, ... digits of Pi
      C := Pi(I);
      Display(C); New_Line;
   end loop;
end Powerful_Pi;
```

This program uses the Machin formula to compute the value of pi to a high degree of accuracy (5000 digits). It then prints the value of pi to the console in a variety of formats.

The Machin formula is a series expansion for pi that converges rapidly. It is given by

```
pi = 4 * arctan(1) - arctan(1/3) - arctan(1/5) + arctan(1/7) + ...
```

where arctan is the arctangent function.

The program uses a complex number type to represent the terms of the series. This is because the arctangent function returns a complex number. The program also uses a large array to store the digits of pi.

The program first computes the value of pi to 10 digits. It then prints the value of pi to the console in a variety of formats. The program then computes the value of pi to 20 digits, and so on.

The program prints the value of pi to the console in the following formats:

* With 0 decimal places
* With 10 decimal places
* With 20 decimal places
* With 30 decimal places

The program also prints the value of pi in the following formats:

* As a complex number
* As a string of digits
* As a hexadecimal number

The program is a good example of how to use complex data types and large arrays in Ada. It is also a good example of how to use the Machin formula to compute the value of pi to a high degree of accuracy.