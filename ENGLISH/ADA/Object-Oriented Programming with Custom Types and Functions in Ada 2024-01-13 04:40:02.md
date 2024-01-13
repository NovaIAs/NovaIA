```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Complex_Code is
   type Point_2D is record
      X : Float;
      Y : Float;
   end record;

   type Point_3D is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;

   type Shape is (Circle, Square, Triangle);

   type Colored_Shape is record
      Shape : Shape;
      Color : Color;
   end record;

   type Color is (Red, Green, Blue, Yellow, Black, White);

   function Area(S : Shape) return Float is
   begin
      case S is
         when Circle => return Pi * Radius**2;
         when Square => return Side**2;
         when Triangle => return 0.5 * Base * Height;
      end case;
   end Area;

   function Perimeter(S : Shape) return Float is
   begin
      case S is
         when Circle => return 2 * Pi * Radius;
         when Square => return 4 * Side;
         when Triangle => return Base + Height + Hypotenuse;
      end case;
   end Perimeter;

   function Draw(S : Colored_Shape) is
   begin
      case S.Shape is
         when Circle => Put("Circle");
         when Square => Put("Square");
         when Triangle => Put("Triangle");
      end case;
      Put(" with color ");
      case S.Color is
         when Red => Put("Red");
         when Green => Put("Green");
         when Blue => Put("Blue");
         when Yellow => Put("Yellow");
         when Black => Put("Black");
         when White => Put("White");
      end case;
      New_Line;
   end Draw;

begin
   declare
      My_Point_2D : Point_2D := (1.0, 2.0);
      My_Point_3D : Point_3D := (3.0, 4.0, 5.0);
      My_Shape : Shape := Circle;
      My_Colored_Shape : Colored_Shape := (Circle, Red);
   begin
      Put("The area of the circle is ");
      Put(Area(Circle), 2, 0);
      New_Line;
      Put("The perimeter of the square is ");
      Put(Perimeter(Square), 2, 0);
      New_Line;
      Put("The 2D point is (");
      Put(My_Point_2D.X, 2, 0);
      Put(", ");
      Put(My_Point_2D.Y, 2, 0);
      Put(")");
      New_Line;
      Put("The 3D point is (");
      Put(My_Point_3D.X, 2, 0);
      Put(", ");
      Put(My_Point_3D.Y, 2, 0);
      Put(", ");
      Put(My_Point_3D.Z, 2, 0);
      Put(")");
      New_Line;
      Draw(My_Colored_Shape);
   end;
end Complex_Code;
```

Explanation:

1. Define custom types:
   - Point_2D: A record to represent a 2D point with X and Y coordinates.
   - Point_3D: A record to represent a 3D point with X, Y, and Z coordinates.
   - Shape: An enumeration type to represent different shapes (Circle, Square, Triangle).
   - Colored_Shape: A record to represent a shape with a color.

2. Define functions:
   - Area: Calculates the area of a shape based on its type (Circle, Square, Triangle).
   - Perimeter: Calculates the perimeter of a shape based on its type.

3. Define a procedure Draw to print the type and color of a Colored_Shape.

4. In the main program:
   - Declare variables of different types, including a 2D point, a 3D point, a shape, and a colored shape.
   - Use the Area and Perimeter functions to calculate and print the area and perimeter of the circle and square.
   - Print the 2D and 3D points.
   - Call the Draw procedure to print the type and color of the colored shape.

This code demonstrates the use of custom types, enumeration types, functions, and procedures in Ada. It also includes examples of record types and case statements for handling different types of shapes.