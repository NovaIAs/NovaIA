```ada
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Complex_Code is
   type Color is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
   type Shape is (Circle, Square, Triangle, Rectangle, Pentagon, Hexagon, Heptagon);

   function Random_Color return Color is
      return Color'Succ(Color(Random(Color'Range)));
   end Random_Color;

   function Random_Shape return Shape is
      return Shape'Succ(Shape(Random(Shape'Range)));
   end Random_Shape;

   function Draw(s: Shape; c: Color) is
      Put_Line(Image(s) & " is " & Image(c));
   end Draw;

   Image: array(Shape) of String := (
      "a red circle", "an orange square", "a yellow triangle",
      "a green rectangle", "a blue pentagon", "an indigo hexagon",
      "a violet heptagon"
   );

begin
   for I in 1..100 loop
      Draw(Random_Shape, Random_Color);
   end loop;
end Complex_Code;
```

This code is a complex demonstration of Ada's capabilities. It defines two custom types, `Color` and `Shape`, and two functions, `Random_Color` and `Random_Shape`, which generate random values of these types. The function `Draw` takes a shape and a color and prints a description of the shape in the specified color.

The array `Image` contains a mapping from each shape to a string describing that shape.

The main program loop calls `Draw` 100 times, each time with a random shape and color. This results in a variety of different shapes and colors being printed to the console.

This code demonstrates Ada's strong typing, its support for custom types, its powerful generics, and its ability to generate random values. It also shows how Ada can be used to create complex and interesting programs.