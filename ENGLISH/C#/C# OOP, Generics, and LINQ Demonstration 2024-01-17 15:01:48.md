```c#
// This program is a complex demonstration of various C# features, including object-oriented programming, generics, and LINQ.

// Define a base class called "Shape"
public abstract class Shape
{
    // Define abstract methods for calculating area and perimeter
    public abstract double CalculateArea();
    public abstract double CalculatePerimeter();
}

// Define a derived class called "Rectangle" that inherits from "Shape"
public class Rectangle : Shape
{
    // Define private fields for length and width
    private double length;
    private double width;

    // Define a constructor to initialize the length and width
    public Rectangle(double length, double width)
    {
        this.length = length;
        this.width = width;
    }

    // Override the abstract methods from the "Shape" class
    public override double CalculateArea()
    {
        return length * width;
    }

    public override double CalculatePerimeter()
    {
        return 2 * (length + width);
    }
}

// Define a derived class called "Circle" that inherits from "Shape"
public class Circle : Shape
{
    // Define a private field for the radius
    private double radius;

    // Define a constructor to initialize the radius
    public Circle(double radius)
    {
        this.radius = radius;
    }

    // Override the abstract methods from the "Shape" class
    public override double CalculateArea()
    {
        return Math.PI * radius * radius;
    }

    public override double CalculatePerimeter()
    {
        return 2 * Math.PI * radius;
    }
}

// Define a generic class called "ShapeCollection" that can store a collection of shapes
public class ShapeCollection<T> where T : Shape
{
    // Define a private field for the collection of shapes
    private List<T> shapes;

    // Define a constructor to initialize the collection
    public ShapeCollection()
    {
        shapes = new List<T>();
    }

    // Define a method to add a shape to the collection
    public void Add(T shape)
    {
        shapes.Add(shape);
    }

    // Define a method to calculate the total area of all shapes in the collection
    public double CalculateTotalArea()
    {
        double totalArea = 0;
        foreach (T shape in shapes)
        {
            totalArea += shape.CalculateArea();
        }
        return totalArea;
    }

    // Define a method to calculate the total perimeter of all shapes in the collection
    public double CalculateTotalPerimeter()
    {
        double totalPerimeter = 0;
        foreach (T shape in shapes)
        {
            totalPerimeter += shape.CalculatePerimeter();
        }
        return totalPerimeter;
    }
}

// Define a main class to demonstrate the usage of the above classes
class Program
{
    static void Main(string[] args)
    {
        // Create a rectangle object
        Rectangle rectangle = new Rectangle(10, 5);

        // Create a circle object
        Circle circle = new Circle(5);

        // Create a shape collection to store the rectangle and circle objects
        ShapeCollection<Shape> shapes = new ShapeCollection<Shape>();
        shapes.Add(rectangle);
        shapes.Add(circle);

        // Calculate the total area and perimeter of all shapes in the collection using LINQ
        double totalArea = shapes.Sum(shape => shape.CalculateArea());
        double totalPerimeter = shapes.Sum(shape => shape.CalculatePerimeter());

        // Display the results
        Console.WriteLine("Total Area: {0}", totalArea);
        Console.WriteLine("Total Perimeter: {0}", totalPerimeter);
    }
}
```

Explanation:

1. We define a base class called "Shape" with abstract methods for calculating area and perimeter.

2. We define two derived classes, "Rectangle" and "Circle", which inherit from the "Shape" class. These classes implement the abstract methods from "Shape" to calculate area and perimeter for their respective shapes.

3. We define a generic class called "ShapeCollection" that can store a collection of shapes.

4. We create a main method to demonstrate the usage of the above classes. We create instances of "Rectangle" and "Circle", add them to a "ShapeCollection", and then use LINQ to calculate the total area and perimeter of all shapes in the collection.

5. We display the results to the console.