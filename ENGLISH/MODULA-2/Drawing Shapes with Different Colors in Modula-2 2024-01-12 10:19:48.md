```modula-2
MODULE Complex;
FROM FormatString IMPORT FormatString;

TYPE Color = (red, green, blue);
TYPE Point = RECORD
    x, y: INTEGER;
END;
TYPE Shape = (circle, square, triangle);

PROCEDURE DrawShape(color: Color; shape: Shape; pos: Point);
VAR
    radius: INTEGER;
    sideLength: INTEGER;
BEGIN
    CASE shape OF
        circle:
            radius := 10;
            FormatString("Drawing a circle at (%d, %d) with radius %d and color %s",
                pos.x, pos.y, radius, ColorToString(color));
        square:
            sideLength := 20;
            FormatString("Drawing a square at (%d, %d) with side length %d and color %s",
                pos.x, pos.y, sideLength, ColorToString(color));
        triangle:
            FormatString("Drawing a triangle at (%d, %d) with color %s",
                pos.x, pos.y, ColorToString(color));
    END;
END DrawShape;

PROCEDURE ColorToString(color: Color): ARRAY OF CHAR;
BEGIN
    CASE color OF
        red: RETURN "red";
        green: RETURN "green";
        blue: RETURN "blue";
    END;
END ColorToString;

PROCEDURE Main;
VAR
    color: Color;
    shape: Shape;
    pos: Point;
BEGIN
    color := blue;
    shape := square;
    pos.x := 100;
    pos.y := 200;
    DrawShape(color, shape, pos);
END Main.
```

Explanation:

This Modula-2 program defines a module called "Complex" that contains various types and procedures for drawing different shapes with different colors at specified positions.

1. Types:
   - Color: An enumerated type representing the three primary colors: red, green, and blue.
   - Point: A record type representing a point in a 2D space, with two integer fields: x and y.
   - Shape: An enumerated type representing three different shapes: circle, square, and triangle.

2. Procedure DrawShape:
   - This procedure takes three parameters: color, shape, and pos.
   - It uses a CASE statement to determine the type of shape and then draws the shape accordingly.
   - For circles, it draws a circle with the specified radius and color at the given position.
   - For squares, it draws a square with the specified side length and color at the given position.
   - For triangles, it simply draws a triangle with the specified color at the given position.

3. Procedure ColorToString:
   - This procedure takes a color as input and returns a string representing the color name.
   - It uses a CASE statement to determine the color and then returns the corresponding string.

4. Procedure Main:
   - This is the main procedure of the program.
   - It initializes variables for color, shape, and position.
   - It then calls the DrawShape procedure to draw a blue square at position (100, 200).

When you run this program, it will draw a blue square at the specified position.