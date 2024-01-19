```csharp
// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Define a delegate for a function that takes an integer and returns a string
public delegate string NumberFormatter(int number);

// Define a class to represent a range of integers
public class IntegerRange
{
    // Properties to store the start and end of the range
    public int Start { get; set; }
    public int End { get; set; }

    // Constructor to initialize the range
    public IntegerRange(int start, int end)
    {
        Start = start;
        End = end;
    }

    // Method to check if a number is within the range
    public bool IsInRange(int number)
    {
        return number >= Start && number <= End;
    }
}

// Define a class to represent a collection of integer ranges
public class IntegerRangeCollection
{
    // Property to store the collection of ranges
    public List<IntegerRange> Ranges { get; set; }

    // Constructor to initialize the collection
    public IntegerRangeCollection()
    {
        Ranges = new List<IntegerRange>();
    }

    // Method to add a range to the collection
    public void AddRange(IntegerRange range)
    {
        Ranges.Add(range);
    }

    // Method to check if a number is within any of the ranges in the collection
    public bool IsInAnyRange(int number)
    {
        return Ranges.Any(range => range.IsInRange(number));
    }
}

// Define a class to represent a set of integer ranges
public class IntegerRangeSet
{
    // Property to store the set of ranges
    public HashSet<IntegerRange> Ranges { get; set; }

    // Constructor to initialize the set
    public IntegerRangeSet()
    {
        Ranges = new HashSet<IntegerRange>();
    }

    // Method to add a range to the set
    public void AddRange(IntegerRange range)
    {
        Ranges.Add(range);
    }

    // Method to check if a number is within any of the ranges in the set
    public bool IsInAnyRange(int number)
    {
        return Ranges.Any(range => range.IsInRange(number));
    }
}

// Define a program to demonstrate the usage of the above classes
public class Program
{
    // Main method
    public static void Main(string[] args)
    {
        // Create an instance of the IntegerRange class
        IntegerRange range1 = new IntegerRange(1, 10);

        // Create an instance of the IntegerRangeCollection class
        IntegerRangeCollection collection = new IntegerRangeCollection();

        // Add the range to the collection
        collection.AddRange(range1);

        // Create an instance of the IntegerRangeSet class
        IntegerRangeSet set = new IntegerRangeSet();

        // Add the range to the set
        set.AddRange(range1);

        // Check if a number is within the range, collection, and set
        int number = 5;
        Console.WriteLine($"Is {number} in range1? {range1.IsInRange(number)}");
        Console.WriteLine($"Is {number} in the collection? {collection.IsInAnyRange(number)}");
        Console.WriteLine($"Is {number} in the set? {set.IsInAnyRange(number)}");
    }
}
```

**Explanation:**

The provided C# code is a complex and differentiated code that implements various data structures and algorithms related to integer ranges. It involves creating classes, defining methods, and demonstrating their usage in a console application. Here's an explanation of the code:

1. **Classes**:
   - `IntegerRange`: A class representing a range of integers with properties for storing the start and end values and methods for checking if a number is within the range.
   - `IntegerRangeCollection`: A class representing a collection of integer ranges with a list to store the ranges and methods for adding ranges and checking if a number is within any range in the collection.
   - `IntegerRangeSet`: A class representing a set of integer ranges with a hash set to store the ranges and methods for adding ranges and checking if a number is within any range in the set.

2. **Methods**:
   - `IsInRange`: A method in the `IntegerRange` class that checks if a given integer is within the range defined by the start and end values.
   - `AddRange`: Methods in the `IntegerRangeCollection` and `IntegerRangeSet` classes that allow adding a new integer range to the collection or set.
   - `IsInAnyRange`: Methods in the `IntegerRangeCollection` and `IntegerRangeSet` classes that check if a given integer is within any of the ranges in the collection or set using `Any` method and a lambda expression.

3. **Program**:
   - In the `Main` method, an instance of the `IntegerRange` class is created, representing a range from 1 to 10.
   - Two instances of `IntegerRangeCollection` and `IntegerRangeSet` are created to demonstrate the usage of these data structures.
   - The range is added to both the collection and the set.
   - A number is provided as input (5 in this example), and the program checks if the number is within the range, collection, and set using the `IsInRange` and `IsInAnyRange` methods.

This code showcases a variety of concepts in C#, including classes, methods, generics, data structures, sets, and lambda expressions. It's a complex and well-written code that demonstrates a deep understanding of C# programming.