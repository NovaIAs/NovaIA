```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure Calculate_Pi(N_Terms : in Integer; Pi : out Float) is

   -- Declare the local variables.
   Doubling : Float := 1.0;
   Term : Float := 1.0;
   Sign : Boolean := True;

begin
   -- Initialize the value of pi.
   Pi := 0.0;

   -- Calculate the terms of the series.
   for I in 1 .. N_Terms loop
      if Sign then
         Pi := Pi + Term;
      else
         Pi := Pi - Term;
      end if;

      -- Update the values of the variables.
      Doubling := Doubling * 2.0;
      Term := Term / Doubling;
      Sign := not Sign;
   end loop;
end Calculate_Pi;

procedure Print_Pi(Pi : Float) is
begin
   -- Print the value of pi.
   Ada.Integer_Text_IO.Put(Pi'Image);
   Ada.Text_IO.New_Line;
end Print_Pi;

procedure Main is

   -- Declare the local variables.
   N_Terms : Integer := 100000;
   Pi : Float;

begin
   -- Calculate the value of pi.
   Calculate_Pi(N_Terms, Pi);

   -- Print the value of pi.
   Print_Pi(Pi);
end Main;
```

This code calculates the value of pi using the Leibniz formula. The Leibniz formula is an infinite series that converges to the value of pi. The code calculates the first N_Terms terms of the series and then adds them up to get an approximation of pi.

The code first declares the local variables. The variable N_Terms is the number of terms to calculate in the series. The variable Pi is the output variable that will contain the calculated value of pi.

The next step is to initialize the value of pi to 0.0.

The code then enters a loop that calculates the terms of the series. The loop iterates from 1 to N_Terms.

Inside the loop, the code first checks the value of the variable Sign. If Sign is True, then the code adds the current term to the value of pi. If Sign is False, then the code subtracts the current term from the value of pi.

After updating the value of pi, the code updates the values of the variables Doubling, Term, and Sign. Doubling is doubled, Term is divided by Doubling, and Sign is negated.

After the loop, the code has calculated the first N_Terms terms of the series and added them up to get an approximation of pi.

The code then calls the procedure Print_Pi to print the value of pi to the console.

The procedure Print_Pi simply prints the value of pi to the console using the Ada.Integer_Text_IO.Put procedure.

The procedure Main calls the procedures Calculate_Pi and Print_Pi to calculate and print the value of pi.