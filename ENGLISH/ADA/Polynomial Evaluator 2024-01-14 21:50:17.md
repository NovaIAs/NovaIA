```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Map;
with Ada.Strings.Fixed;

procedure Poly is

   type Variable is (X, Y);

   function Get_Coefficient (Index : Variable) return Float is
   (
      if Index = X then
         return 1.0;
      elsif Index = Y then
         return 2.0;
      else
         return 0.0;
      end if;
   );

   function Evaluate (Poly : array (Variable) of Float; X, Y : Float) return Float is
   (
      declare
         Sum : Float := 0.0;
      begin
         for Index in Variable'range loop
            Sum := Sum + Poly (Index) * Get_Coefficient (Index);
         end loop;
         return Sum;
      end;
   );

begin
   declare
      Poly : array (Variable) of Float := (1.0, 2.0);
      X, Y : Float;
   begin
      Ada.Text_IO.Put_Line ("Enter the values of X and Y:");
      Ada.Float_Text_IO.Get (Ada.Command_Line.Argument (1), X);
      Ada.Float_Text_IO.Get (Ada.Command_Line.Argument (2), Y);
      Ada.Text_IO.Put_Line ("The value of the polynomial is: ");
      Ada.Float_Text_IO.Put (Evaluate (Poly, X, Y));
      Ada.Text_IO.New_Line;
   end;
end Poly;
```

This code implements a polynomial expression in two variables, X and Y, and evaluates it for given values of X and Y.

The `Variable` type defines the variables used in the polynomial expression.

The `Get_Coefficient` function returns the coefficient of the given variable in the polynomial expression.

The `Evaluate` function evaluates the polynomial expression for the given values of X and Y.

The main program declares an array of coefficients for the polynomial expression and calls the `Evaluate` function to evaluate the polynomial expression for the given values of X and Y.

The output of the program is the value of the polynomial expression for the given values of X and Y.