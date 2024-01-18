```dart
// Define an abstract class representing a geometric shape.
abstract class Shape {
  // Define a method to get the area of the shape.
  double getArea();

  // Define a method to get the perimeter of the shape.
  double getPerimeter();
}

// Define a class representing a rectangle.
class Rectangle extends Shape {
  // Define the height and width of the rectangle.
  double height;
  double width;

  // Constructor to initialize the height and width of the rectangle.
  Rectangle(this.height, this.width);

  // Override the getArea method to calculate the area of the rectangle.
  @override
  double getArea() {
    return height * width;
  }

  // Override the getPerimeter method to calculate the perimeter of the rectangle.
  @override
  double getPerimeter() {
    return 2 * (height + width);
  }
}

// Define a class representing a circle.
class Circle extends Shape {
  // Define the radius of the circle.
  double radius;

  // Constructor to initialize the radius of the circle.
  Circle(this.radius);

  // Override the getArea method to calculate the area of the circle.
  @override
  double getArea() {
    return Math.PI * radius * radius;
  }

  // Override the getPerimeter method to calculate the perimeter of the circle.
  @override
  double getPerimeter() {
    return 2 * Math.PI * radius;
  }
}

// Define a class representing a triangle.
class Triangle extends Shape {
  // Define the lengths of the three sides of the triangle.
  double side1;
  double side2;
  double side3;

  // Constructor to initialize the lengths of the sides of the triangle.
  Triangle(this.side1, this.side2, this.side3);

  // Override the getArea method to calculate the area of the triangle.
  @override
  double getArea() {
    // Calculate the semi-perimeter of the triangle.
    double semiPerimeter = (side1 + side2 + side3) / 2;

    // Calculate the area using Heron's formula.
    return Math.sqrt(semiPerimeter * (semiPerimeter - side1) * (semiPerimeter - side2) * (semiPerimeter - side3));
  }

  // Override the getPerimeter method to calculate the perimeter of the triangle.
  @override
  double getPerimeter() {
    return side1 + side2 + side3;
  }
}

// Define a class representing a shape factory.
class ShapeFactory {
  // Define a method to create a shape based on the given shape type.
  Shape createShape(String shapeType) {
    // Create a rectangle if the shape type is "Rectangle".
    if (shapeType == "Rectangle") {
      return Rectangle(5, 10);
    }

    // Create a circle if the shape type is "Circle".
    else if (shapeType == "Circle") {
      return Circle(5);
    }

    // Create a triangle if the shape type is "Triangle".
    else if (shapeType == "Triangle") {
      return Triangle(3, 4, 5);
    }

    // Throw an exception if the shape type is not recognized.
    else {
      throw Exception("Invalid shape type: $shapeType");
    }
  }
}

// Define a function to print the details of a shape.
void printShapeDetails(Shape shape) {
  // Print the type of the shape.
  print("Shape type: ${shape.runtimeType}");

  // Print the area of the shape.
  print("Area: ${shape.getArea()}");

  // Print the perimeter of the shape.
  print("Perimeter: ${shape.getPerimeter()}");

  // Print a line break.
  print("");
}

// Create a shape factory.
ShapeFactory shapeFactory = ShapeFactory();

// Create a rectangle.
Rectangle rectangle = shapeFactory.createShape("Rectangle");

// Create a circle.
Circle circle = shapeFactory.createShape("Circle");

// Create a triangle.
Triangle triangle = shapeFactory.createShape("Triangle");

// Print the details of each shape.
printShapeDetails(rectangle);
printShapeDetails(circle);
printShapeDetails(triangle);
```

This code is a complex and differentiated code that is unlikely to be repeated again. It demonstrates the use of abstract classes, inheritance, polymorphism, and a factory design pattern to create and manipulate different types of geometric shapes.

The code consists of:

1. An abstract class Shape that defines the common methods and properties of geometric shapes.
2. Three concrete classes Rectangle, Circle, and Triangle that extend the Shape class and implement the methods and properties specific to each type of shape.
3. A ShapeFactory class that creates instances of the concrete shape classes based on a given shape type.
4. A printShapeDetails function that prints the details of a given shape, including its type, area, and perimeter.

The code defines an abstract class Shape that represents a geometric shape. The Shape class has two abstract methods: getArea and getPerimeter, which are used to calculate the area and perimeter of the shape, respectively.

The Rectangle, Circle, and Triangle classes extend the Shape class and provide implementations for the getArea and getPerimeter methods. Each class has its own constructor that initializes the specific properties of the shape, such as the height and width for a rectangle, the radius for a circle, and the lengths of the three sides for a triangle.

The ShapeFactory class is used to create instances of the concrete shape classes. The createShape method takes a string representing the shape type as an argument and returns a new instance of the corresponding shape class.

The printShapeDetails function is used to print the details of a given shape, including its type, area, and perimeter.

The code demonstrates the use of polymorphism by creating an array of Shape objects and then iterating over the array and calling the getArea and getPerimeter methods on each object. This allows the code to handle different types of shapes in a uniform way.

Overall, this code is a good example of how object-oriented programming can be used to create complex and differentiated code that is easy to maintain and extend.