```ada

-- This program implements a simple calculator using generics.
-- It can perform addition, subtraction, multiplication, and division on
-- any type of data that supports the "+" and "-" operators.

generic
   type Number is private;
   with function "+" (Left, Right : Number) return Number;
   with function "-" (Left, Right : Number) return Number;
   with function "*" (Left, Right : Number) return Number;
   with function "/" (Left, Right : Number) return Number;
package Calc is
   function Calculate (Operator : String; Left, Right : Number) return Number;
end Calc;

package body Calc is
   function Calculate (Operator : String; Left, Right : Number) return Number is
      begin
         if Operator = "+" then
            return Left + Right;
         elsif Operator = "-" then
            return Left - Right;
         elsif Operator = "*" then
            return Left * Right;
         elsif Operator = "/" then
            return Left / Right;
         else
            raise Program_Error("Invalid operator.");
         end if;
      end Calculate;
end Calc;

with Ada.Text_IO;

procedure Main is
   use Ada.Text_IO;

   X : Integer := 10;
   Y : Integer := 5;

   Z : Float := 3.14;
   W : Float := 2.71;

   begin
      Put_Line("Integer addition:");
      Put("X + Y = "); Put(Calc.Calculate("+", X, Y)); New_Line;

      Put_Line("Integer subtraction:");
      Put("X - Y = "); Put(Calc.Calculate("-", X, Y)); New_Line;

      Put_Line("Integer multiplication:");
      Put("X * Y = "); Put(Calc.Calculate("*", X, Y)); New_Line;

      Put_Line("Integer division:");
      Put("X / Y = "); Put(Calc.Calculate("/", X, Y)); New_Line;

      Put_Line("Float addition:");
      Put("Z + W = "); Put(Calc.Calculate("+", Z, W)); New_Line;

      Put_Line("Float subtraction:");
      Put("Z - W = "); Put(Calc.Calculate("-", Z, W)); New_Line;

      Put_Line("Float multiplication:");
      Put("Z * W = "); Put(Calc.Calculate("*", Z, W)); New_Line;

      Put_Line("Float division:");
      Put("Z / W = "); Put(Calc.Calculate("/", Z, W)); New_Line;
   end Main;

```

**Explanation:**

This code implements a simple calculator using generics. The generic package `Calc` defines a function `Calculate` that can perform addition, subtraction, multiplication, and division on any type of data that supports the "+" and "-" operators. The body of the package contains the implementation of the `Calculate` function.

The main program `Main` uses the `Calc` package to perform some calculations on integer and float numbers. The results of the calculations are printed to the console.

Here are some explanations of the code:

* The `generic` statement defines a generic package. The package parameters are:
    * `type Number is private;`: This parameter specifies the type of data that the `Calc` package will be able to operate on. The type must be private, which means that its implementation details are hidden from the user.
    * `with function "+" (Left, Right : Number) return Number;`: This parameter specifies a function that can be used to add two numbers of type `Number`.
    * `with function "-" (Left, Right : Number) return Number;`: This parameter specifies a function that can be used to subtract two numbers of type `Number`.
    * `with function "*" (Left, Right : Number) return Number;`: This parameter specifies a function that can be used to multiply two numbers of type `Number`.
    * `with function "/" (Left, Right : Number) return Number;`: This parameter specifies a function that can be used to divide two numbers of type `Number`.

* The `package Calc is` statement defines the body of the `Calc` package. The package contains a single function, `Calculate`.

* The `function Calculate` function takes three parameters:
    * `Operator : String;`: This parameter specifies the operator to be used in the calculation. It must be one of "+", "-", "*", or "/".
    * `Left, Right : Number;`: These parameters specify the two numbers to be used in the calculation.

* The body of the `Calculate` function uses a `case` statement to select the appropriate calculation to perform based on the value of the `Operator` parameter.

* The `Main` program uses the `Calc` package to perform some calculations on integer and float numbers. The results of the calculations are printed to the console.

This code can be used to perform simple calculations on any type of data that supports the "+" and "-" operators. It is a good example of how generics can be used to create reusable code.