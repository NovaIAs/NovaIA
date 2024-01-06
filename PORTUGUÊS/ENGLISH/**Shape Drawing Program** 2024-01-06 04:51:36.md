```smalltalk
"Create a class called 'Point' with two instance variables, 'x' and 'y'"
Object subclass: Point
    instanceVariableNames: 'x y'

"Define a method called 'initialize' to initialize the 'x' and 'y' instance variables"
Point class >> initialize: x y
    super initialize.
    x := x.
    y := y.

"Create a class called 'Line' with two instance variables, 'start' and 'end'"
Object subclass: Line
    instanceVariableNames: 'start end'

"Define a method called 'initialize' to initialize the 'start' and 'end' instance variables"
Line class >> initialize: start end
    super initialize.
    start := start.
    end := end.

"Create a class called 'Rectangle' with four instance variables, 'origin', 'width', 'height', and 'color'"
Object subclass: Rectangle
    instanceVariableNames: 'origin width height color'

"Define a method called 'initialize' to initialize the 'origin', 'width', 'height', and 'color' instance variables"
Rectangle class >> initialize: origin width height color
    super initialize.
    origin := origin.
    width := width.
    height := height.
    color := color.

"Create a class called 'Circle' with two instance variables, 'center' and 'radius'"
Object subclass: Circle
    instanceVariableNames: 'center radius'

"Define a method called 'initialize' to initialize the 'center' and 'radius' instance variables"
Circle class >> initialize: center radius
    super initialize.
    center := center.
    radius := radius.

"Create a class called 'Canvas' with an instance variable called 'shapes'"
Object subclass: Canvas
    instanceVariableNames: 'shapes'

"Define a method called 'initialize' to initialize the 'shapes' instance variable"
Canvas class >> initialize
    super initialize.
    shapes := OrderedCollection new.

"Define a method called 'addShape:' to add a shape to the 'shapes' collection"
Canvas class >> addShape: aShape
    shapes add: aShape.

"Define a method called 'draw' to draw all of the shapes in the 'shapes' collection"
Canvas class >> draw
    shapes do: [:shape | shape draw].

"Create a class called 'Shape' that is a superclass for all of the other shape classes"
Object subclass: Shape
    classVariableNames: 'allShapes'

"Define a class method called 'allShapes' that returns a collection of all of the shape classes"
Shape class >> allShapes
    ^allShapes

"Initialize the 'allShapes' class variable with a collection of all of the shape classes"
Shape class >> initialize
    allShapes := OrderedCollection new.
    allShapes add: Point.
    allShapes add: Line.
    allShapes add: Rectangle.
    allShapes add: Circle.

"Define a method called 'draw' that is implemented by all of the shape subclasses"
Shape class >> draw
    self error: 'This method must be implemented by subclasses.'

"Create an instance of the 'Canvas' class"
canvas := Canvas new.

"Add some shapes to the 'canvas'"
canvas addShape: (Point new: 100@100).
canvas addShape: (Line new: Point new: 100@100 to: Point new: 200@200).
canvas addShape: (Rectangle new: Point new: 100@100 width: 100 height: 200 color: Color yellow).
canvas addShape: (Circle new: Point new: 150@150 radius: 50).

"Draw the shapes on the 'canvas'"
canvas draw.
```

This code creates a class hierarchy for representing and drawing different types of shapes, such as points, lines, rectangles, and circles. It defines a `Canvas` class that can hold a collection of shapes and draw them all at once.

The `Shape` class is a superclass for all of the other shape classes and defines a class method `allShapes` that returns a collection of all of the shape classes. The `draw` method is implemented by all of the shape subclasses and is used to draw the shape on the canvas.

The code then creates an instance of the `Canvas` class and adds some shapes to it, including a point, a line, a rectangle, and a circle. Finally, the `draw` method is called on the canvas to draw all of the shapes.