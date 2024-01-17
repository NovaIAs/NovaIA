```smalltalk

Object subclass: Rectangle [
  | width height |
  "Instance Variables"
  width := 0.
  height := 0.

  "Class Methods"
  class>>newWithWidth: [ :aWidth ]
    height: [ :aHeight ]
    ^ self new width: aWidth height: aHeight.

  "Instance Methods"
  width: [ :aWidth ]
    width := aWidth.

  height: [ :aHeight ]
    height := aHeight.

  width
    ^ width.

  height
    ^ height.

  area
    ^ width * height.
]

Object subclass: Square [
  | side |
  "Instance Variables"
  side := 0.

  "Class Methods"
  class>>newWithSide: [ :aSide ]
    ^ self new side: aSide.

  "Instance Methods"
  side: [ :aSide ]
    side := aSide.

  side
    ^ side.

  area
    ^ side * side.
]

Object subclass: Circle [
  | radius |
  "Instance Variables"
  radius := 0.

  "Class Methods"
  class>>newWithRadius: [ :aRadius ]
    ^ self new radius: aRadius.

  "Instance Methods"
  radius: [ :aRadius ]
    radius := aRadius.

  radius
    ^ radius.

  area
    ^ 3.1415926 * radius * radius.
]

Object subclass: Shape [
  | color |
  "Instance Variables"
  color := 'white'.

  "Class Methods"
  class>>new
    ^ self new color: 'white'.

  class>>newColor: [ :aColor ]
    ^ self new color: aColor.

  "Instance Methods"
  color: [ :aColor ]
    color := aColor.

  color
    ^ color.

  draw
    self subclassResponsibility.
]

Object subclass: RectangleView [
  | myRectangle |
  "Instance Variables"
  myRectangle := Rectangle newWithWidth: 100 height: 200.

  "Class Methods"
  class>>new
    ^ self new rectangle: Rectangle newWithWidth: 100 height: 200.

  class>>newRectangle: [ :aRectangle ]
    ^ self new rectangle: aRectangle.

  "Instance Methods"
  rectangle: [ :aRectangle ]
    myRectangle := aRectangle.

  rectangle
    ^ myRectangle.

  draw
    myRectangle color := 'red'.
    super draw.
]

Object subclass: SquareView [
  | mySquare |
  "Instance Variables"
  mySquare := Square newWithSide: 50.

  "Class Methods"
  class>>new
    ^ self new square: Square newWithSide: 50.

  class>>newSquare: [ :aSquare ]
    ^ self new square: aSquare.

  "Instance Methods"
  square: [ :aSquare ]
    mySquare := aSquare.

  square
    ^ mySquare.

  draw
    mySquare color := 'orange'.
    super draw.
]

Object subclass: CircleView [
  | myCircle |
  "Instance Variables"
  myCircle := Circle newWithRadius: 75.

  "Class Methods"
  class>>new
    ^ self new circle: Circle newWithRadius: 75.

  class>>newCircle: [ :aCircle ]
    ^ self new circle: aCircle.

  "Instance Methods"
  circle: [ :aCircle ]
    myCircle := aCircle.

  circle
    ^ myCircle.

  draw
    myCircle color := 'green'.
    super draw.
]

Object subclass: DrawingCanvas [
  | shapes |
  "Instance Variables"
  shapes := OrderedCollection new.

  "Class Methods"
  class>>new
    ^ self new shapes: OrderedCollection new.

  class>>newShapes: [ :aCollection ]
    ^ self new shapes: aCollection.

  "Instance Methods"
  shapes: [ :aCollection ]
    shapes := aCollection.

  shapes
    ^ shapes.

  addShape: [ :aShape ]
    shapes add: aShape.

  removeShape: [ :aShape ]
    shapes remove: aShape.

  draw
    shapes do: [ :each | each draw ].
]

Object subclass: DrawingApplication [
  | drawingCanvas |
  "Instance Variables"
  drawingCanvas := DrawingCanvas new.

  "Class Methods"
  class>>new
    ^ self new canvas: DrawingCanvas new.

  class>>newCanvas: [ :aCanvas ]
    ^ self new canvas: aCanvas.

  "Instance Methods"
  canvas: [ :aCanvas ]
    drawingCanvas := aCanvas.

  canvas
    ^ drawingCanvas.

  addShape: [ :aShape ]
    drawingCanvas addShape: aShape.

  removeShape: [ :aShape ]
    drawingCanvas removeShape: aShape.

  redraw
    drawingCanvas draw.
]

```

This code defines several classes in Smalltalk, including Rectangle, Square, Circle, Shape, RectangleView, SquareView, CircleView, DrawingCanvas, and DrawingApplication. Here's an explanation of each class:

1. **Rectangle**: Represents a rectangle with width and height properties.

2. **Square**: Represents a square with a side property.

3. **Circle**: Represents a circle with a radius property.

4. **Shape**: Defines common properties and behaviors for different shapes, such as color.

5. **RectangleView**: A view that displays a rectangle and allows for color customization.

6. **SquareView**: Similar to RectangleView but specifically for squares.

7. **CircleView**: Similar to RectangleView but specifically for circles.

8. **DrawingCanvas**: Manages a collection of shapes and provides methods for adding, removing, and drawing them.

9. **DrawingApplication**: Sets up a drawing environment with a drawing canvas and provides methods for adding and removing shapes.

The code demonstrates the use of object-oriented principles, inheritance, and polymorphism to create a simple drawing application that allows users to add, remove, and draw different shapes.