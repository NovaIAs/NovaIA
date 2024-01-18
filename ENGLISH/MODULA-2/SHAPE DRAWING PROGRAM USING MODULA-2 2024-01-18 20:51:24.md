```modula-2
MODULE ShapeGraphics;
FROM Terminal IMPORT WriteStringLn, WriteIntLn, ReadInt;
FROM TextIO IMPORT Open, Close, ReadString;
FROM Random IMPORT Random;
FROM Math IMPORT Sqrt;

TYPE Shape =
  RECORD
    name: ARRAY[1..5] OF CHAR;  (* 5-character name of shape *)
    description: ARRAY[1..15] OF CHAR; (* 15-character description *)
    area: REAL;                     (* area of the shape in square units *)
    perimeter: REAL;                (* perimeter of the shape in linear units *)
    rotatable: BOOLEAN;           (* TRUE if shape is rotatable, FALSE otherwise *)
    draw: PROCEDURE;               (* procedure to draw the shape *)
    isInside: PROCEDURE (x: REAL; y: REAL): BOOLEAN;
                (* procedure that returns TRUE if point (x,y) is inside the shape,
                   FALSE otherwise *)
  END;

TYPE Circle =
  RECORD
    (Shape): Shape;             (* inherit the Shape fields and their types *)
    center: ARRAY[1..2] OF REAL;  (* center of the circle *)
    radius: REAL;                (* radius of the circle *)
  END;

TYPE Square =
  RECORD
    (Shape): Shape;             (* inherit the Shape fields and their types *)
    side: REAL;                   (* length of side of the square *)
    llCorner: ARRAY[1..2] OF REAL; (* coordinates of lower-left corner *)
  END;

TYPE Rectangle =
  RECORD
    (Shape): Shape;             (* inherit the Shape fields and their types *)
    side1: REAL;                  (* length of side 1 of the rectangle *)
    side2: REAL;                  (* length of side 2 of the rectangle *)
    llCorner: ARRAY[1..2] OF REAL; (* coordinates of lower-left corner *)
  END;

TYPE Triangle =
  RECORD
    (Shape): Shape;             (* inherit the Shape fields and their types *)
    vertices: ARRAY[1..3, 1..2] OF REAL; (* coordinates of 3 vertices *)
  END;

PROCEDURE DrawCircle(SELF: Circle);
(* draw the circle (and optionally print out its coordinates) *)
VAR
  x: REAL;
  y: REAL;
  i: CARDINAL;
BEGIN
  WriteStringLn("Drawing a Circle:");
  WriteString("Center: ");
  FOR i:= 1 TO 2 DO WriteReal(SELF.center[i], 2, 0); WriteString(", "); END; WriteLn;
  WriteString("Radius: "); WriteReal(SELF.radius, 2, 0); WriteLn;
  FOR i:= 1 TO 360 DO
    x:= SELF.center[1] + SELF.radius * Cos(i * 2 * PI / 360.0);
    y:= SELF.center[2] + SELF.radius * Sin(i * 2 * PI / 360.0);
    WriteString("@");
  END;
  WriteLn;
END DrawCircle;

PROCEDURE IsInsideCircle(SELF: Circle; x, y: REAL): BOOLEAN;
(* is the point (x,y) inside the circle? *)
VAR
  dx: REAL;
  dy: REAL;
BEGIN
  dx:= x - SELF.center[1];
  dy:= y - SELF.center[2];
  RETURN Sqrt(dx*dx + dy*dy) <= SELF.radius;
END IsInsideCircle;

PROCEDURE DrawSquare(SELF: Square);
(* draw the square (and optionally print out its coordinates) *)
VAR
  i: CARDINAL;
BEGIN
  WriteStringLn("Drawing a Square:");
  WriteString("Lower Left Corner: ");
  FOR i:= 1 TO 2 DO WriteReal(SELF.llCorner[i], 2, 0); WriteString(", "); END; WriteLn;
  WriteString("Side: "); WriteReal(SELF.side, 2, 0); WriteLn;
  WriteString("  "); FOR i:= 1 TO SELF.side DO WriteString("-"); END; WriteLn;
  FOR i:= 1 TO SELF.side DO
    WriteString("|");
    FOR j:= 1 TO SELF.side DO WriteString(" "); END;
    WriteString("|");
    WriteLn;
  END;
  WriteString("  "); FOR i:= 1 TO SELF.side DO WriteString("-"); END; WriteLn;
END DrawSquare;

PROCEDURE IsInsideSquare(SELF: Square; x, y: REAL): BOOLEAN;
(* is the point (x,y) inside the square? *)
VAR
  dx: REAL;
  dy: REAL;
BEGIN
  dx:= x - SELF.llCorner[1];
  dy:= y - SELF.llCorner[2];
  RETURN (0.0 <= dx AND dx <= SELF.side) AND (0.0 <= dy AND dy <= SELF.side);
END IsInsideSquare;

PROCEDURE DrawRectangle(SELF: Rectangle);
(* draw the rectangle (and optionally print out its coordinates) *)
VAR
  i: CARDINAL;
BEGIN
  WriteStringLn("Drawing a Rectangle:");
  WriteString("Lower Left Corner: ");
  FOR i:= 1 TO 2 DO WriteReal(SELF.llCorner[i], 2, 0); WriteString(", "); END; WriteLn;
  WriteString("Side 1: "); WriteReal(SELF.side1, 2, 0); WriteString(", Side 2: ");
  WriteReal(SELF.side2, 2, 0); WriteLn;
  WriteString("  "); FOR i:= 1 TO SELF.side1 DO WriteString("-"); END; WriteLn;
  FOR i:= 1 TO SELF.side2 DO
    WriteString("|");
    FOR j:= 1 TO SELF.side1 DO WriteString(" "); END;
    WriteString("|");
    WriteLn;
  END;
  WriteString("  "); FOR i:= 1 TO SELF.side1 DO WriteString("-"); END; WriteLn;
END DrawRectangle;

PROCEDURE IsInsideRectangle(SELF: Rectangle; x, y: REAL): BOOLEAN;
(* is the point (x,y) inside the rectangle? *)
VAR
  dx: REAL;
  dy: REAL;
BEGIN
  dx:= x - SELF.llCorner[1];
  dy:= y - SELF.llCorner[2];
  RETURN (0.0 <= dx AND dx <= SELF.side1) AND (0.0 <= dy AND dy <= SELF.side2);
END IsInsideRectangle;

PROCEDURE DrawTriangle(SELF: Triangle);
(* draw the triangle (and optionally print out its coordinates) *)
VAR
  i: CARDINAL;
BEGIN
  WriteStringLn("Drawing a Triangle:");
  WriteString("Vertices:");
  FOR i:= 1 TO 3 DO
    WriteString(" ("); WriteReal(SELF.vertices[i,1], 2, 0);
    WriteString(", "); WriteReal(SELF.vertices[i,2], 2, 0); WriteString(")");
  END; WriteLn;
  WriteString("  "); FOR i:= 1 TO 25 DO WriteString("-"); END; WriteLn;
  FOR i:= 1 TO 25 DO
    WriteString("|");
    FOR j:= 1 TO 25 DO WriteString(" "); END;
    WriteString("|");
    WriteLn;
  END;
  WriteString("  "); FOR i:= 1 TO 25 DO WriteString("-"); END; WriteLn;
END DrawTriangle;

PROCEDURE IsInsideTriangle(SELF: Triangle; x, y: REAL): BOOLEAN;
(* is the point (x,y) inside the triangle? *)
VAR
  ax: REAL;  ay: REAL;  bx: REAL;  by: REAL;  cx: REAL;  cy: REAL;
  v0x: REAL;  v0y: REAL; v1x: REAL;  v1y: REAL; v2x: REAL;  v2y: REAL;
  dot00: REAL;  dot01: REAL;  dot02: REAL;  dot11: REAL;  dot12: REAL;
  invDenom: REAL;
  u: REAL;  v: REAL;
BEGIN
  ax:= SELF.vertices[1,1];  ay:= SELF.vertices[1,2];
  bx:= SELF.vertices[2,1];  by:= SELF.vertices[2,2];
  cx:= SELF.vertices[3,1];  cy:= SELF.vertices[3,2];
  v0x:= cx - ax;  v0y:= cy - ay;
  v1x:= bx - ax;  v1y:= by - ay;
  v2x:= x - ax;  v2y:= y - ay;
  dot00:= v0x*v0x + v0y*v0y;
  dot01:= v0x*v1x + v0y*v1y;
  dot02:= v0x*v2x + v0y*v2y;
  dot11:= v1x*v1x + v1y*v1y;
  dot12:= v1x*v2x + v1y*v2y;
  invDenom:= 1.0 / (dot00 * dot11 - dot01 * dot01);
  u:= (dot11 * dot02 - dot01 * dot12) * invDenom;
  v:= (dot00 * dot12 - dot01 * dot02) * invDenom;
  RETURN (u >= 0.0) AND (v >= 0.0) AND (u + v <= 1.0);
END IsInsideTriangle;

PROCEDURE InitializeShapes();
(* initializes the shapes *)
VAR
  i: CARDINAL;
BEGIN
  i:= 1;
  WITH Circle.New(SELF) DO
    BEGIN
      SELF.center[1]:= 10.0; SELF.center[2]:= 10.0; SELF.radius:= 5.0;
      SELF.name:= "Circle"; SELF.area:= PI * SELF.radius * SELF.radius;
      SELF.perimeter:= 2.0 * PI * SELF.radius;
      SELF.drawProc:= @DrawCircle; SELF.isInsideProc:= @IsInsideCircle;
      shapes[i]:= SELF;
    END; i:= i + 1;

  WITH Square.New(SELF) DO
    BEGIN
      SELF.llCorner[1]:= 0.0; SELF.llCorner[2]:= 0.0; SELF.side:= 10.0;
      SELF.name:= "Square"; SELF.area:= SELF.side * SELF.side;
      SELF.perimeter:= 4.0 * SELF.side;
      SELF.drawProc:= @DrawSquare; SELF.isInsideProc:= @IsInsideSquare;
      shapes[i]:= SELF;
    END; i:= i + 1;

  WITH Rectangle.New(SELF) DO
    BEGIN
      SELF.llCorner[1]:= 0.0; SELF.llCorner[2]:= 0.0; SELF.side1:= 10.0;
      SELF.side2:= 5.0; SELF.name:= "Rectangle"; SELF.area:= SELF.side1 * SELF.side2;
      SELF.perimeter:= 2.0 * (SELF.side1 + SELF.side2);
      SELF.drawProc:= @DrawRectangle; SELF.isInsideProc:= @IsInsideRectangle;
      shapes[i]:= SELF;
    END; i:= i + 1;

  WITH Triangle.New(SELF) DO
    BEGIN
      SELF.vertices[1,1]:= 0.0; SELF.vertices[1,2]:= 0.0;
      SELF.vertices[2,1]:= 10.0; SELF.vertices[2,2]:= 0.0;
      SELF.vertices[3,1]:= 5.0; SELF.vertices[3,2]:= 8.660254037844386;
      SELF.name:= "Triangle";
      SELF.area:= SELF.vertices[2,1] * SELF.vertices[3,2] -
                  SELF.vertices[3,1] * SELF.vertices[2,2];
      SELF.area:= ABS(SELF.area / 2.0);
      SELF.perimeter:= SELF.vertices[1,1] - SELF.vertices[2,1] +
                      SELF.vertices[3,1] - SELF.vertices[1,1] +
                      SELF.vertices[2,1] - SELF.vertices[3,1];
      SELF.drawProc:= @DrawTriangle; SELF.isInsideProc:= @IsInsideTriangle;
      shapes[i]:= SELF;
    END;
END InitializeShapes;

PROCEDURE DisplayShape(n: CARDINAL);
(* displays the shape 'n' *)
VAR
  shape: Shape_ptr;
BEGIN
  shape:= shapes[n];
  shape.drawProc(shape);
END DisplayShape;

PROCEDURE ShapeArea(n: CARDINAL): LONGREAL;
(* returns the shape 'n' area *)
VAR
  shape: Shape_ptr;
BEGIN
  shape:= shapes[n];
  RETURN shape.area;
END ShapeArea;

PROCEDURE ShapePerimeter(n: CARDINAL): LONGREAL;
(* returns the shape 'n' perimeter *)
VAR
  shape: Shape_ptr;
BEGIN
  shape:= shapes[n];
  RETURN shape.perimeter;
END ShapePerimeter;

PROCEDURE IsPointInShape(n: CARDINAL; x: LONGREAL; y: LONGREAL): BOOLEAN;
(* returns TRUE if the point (x,y) is inside the shape 'n' *)
VAR
  shape: Shape_ptr;
BEGIN
  shape:= shapes[n];
  RETURN shape.isInsideProc(shape,x,y);
END IsPointInShape;

(* MAIN MODULE *)
MODULE main;
FROM Shape IMPORT Shape_ptr;
FROM Shape IMPORT ShapeArea;
FROM Shape IMPORT ShapePerimeter;
FROM Shape IMPORT IsPointInShape;
FROM Shape IMPORT InitializeShapes;
FROM Shape IMPORT DisplayShape;
FROM Terminal IMPORT WriteInt;
FROM Terminal IMPORT WriteLn;
FROM Terminal IMPORT WriteString;
FROM Terminal IMPORT WriteLongReal;

VAR
  shapes: ARRAY [1..4] OF Shape_ptr;
BEGIN
  InitializeShapes;
  DisplayShape(1);
  DisplayShape(2);
  DisplayShape(3);
  DisplayShape(4);
  WriteLn;
  WriteString("Shape"); WriteInt(1,11); WriteString("Area: ");
  WriteLongReal(ShapeArea(1),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(2,11); WriteString("Area: ");
  WriteLongReal(ShapeArea(2),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(3,11); WriteString("Area: ");
  WriteLongReal(ShapeArea(3),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(4,11); WriteString("Area: ");
  WriteLongReal(ShapeArea(4),10,3,0); WriteLn;
  WriteLn;
  WriteString("Shape"); WriteInt(1,11); WriteString("Perimeter: ");
  WriteLongReal(ShapePerimeter(1),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(2,11); WriteString("Perimeter: ");
  WriteLongReal(ShapePerimeter(2),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(3,11); WriteString("Perimeter: ");
  WriteLongReal(ShapePerimeter(3),10,3,0); WriteLn;
  WriteString("Shape"); WriteInt(4,11); WriteString("Perimeter: ");
  WriteLongReal(ShapePerimeter(4),10,3,0); WriteLn;
  WriteLn;
  WriteString("Is the point (5,5) inside shape 1?: ");
  IF IsPointInShape(1,5,5) THEN WriteString("Yes") ELSE WriteString("No");
  WriteLn;
  WriteString("Is the point (10,10) inside shape 2?: ");
  IF IsPointInShape(2,10,10) THEN WriteString("Yes") ELSE WriteString("No");
  WriteLn;
  WriteString("Is the point (-20,-20) inside shape 3?: ");
  IF IsPointInShape(3,-20,-20) THEN WriteString("Yes") ELSE WriteString("No");
  WriteLn;
  WriteString("Is the point (5.5,8.660254037844386) inside shape 4?: ");
  IF IsPointInShape(4,5.5,8.660254037844386) THEN WriteString("Yes") ELSE
  WriteString("No");
  WriteLn;
END main.