```c#
// Define a delegate type for a function that takes an integer and returns a string.
public delegate string NumberFormatter(int number);

// Define a class to represent a range of integers.
public class IntegerRange
{
    private int _start;
    private int _end;

    public IntegerRange(int start, int end)
    {
        _start = start;
        _end = end;
    }

    // Define a method to format the range as a string.
    public string Format(NumberFormatter formatter)
    {
        StringBuilder builder = new StringBuilder();

        for (int i = _start; i <= _end; i++)
        {
            builder.Append(formatter(i));
            builder.Append(", ");
        }

        // Remove the trailing comma and space.
        builder.Remove(builder.Length - 2, 2);

        return builder.ToString();
    }
}

// Define a class to represent a collection of integer ranges.
public class IntegerRangeCollection
{
    private List<IntegerRange> _ranges;

    public IntegerRangeCollection()
    {
        _ranges = new List<IntegerRange>();
    }

    // Define a method to add a range to the collection.
    public void Add(IntegerRange range)
    {
        _ranges.Add(range);
    }

    // Define a method to format the collection as a string.
    public string Format(NumberFormatter formatter)
    {
        StringBuilder builder = new StringBuilder();

        foreach (IntegerRange range in _ranges)
        {
            builder.Append(range.Format(formatter));
            builder.Append("; ");
        }

        // Remove the trailing semicolon and space.
        builder.Remove(builder.Length - 2, 2);

        return builder.ToString();
    }
}

// Define a class to represent a formatter for integer ranges.
public class IntegerRangeFormatter
{
    private string _format;

    public IntegerRangeFormatter(string format)
    {
        _format = format;
    }

    // Define a method to format an integer range as a string.
    public string Format(IntegerRange range)
    {
        return string.Format(_format, range._start, range._end);
    }
}

// Define a class to represent a program that demonstrates the use of the IntegerRange, IntegerRangeCollection, and IntegerRangeFormatter classes.
public class Program
{
    public static void Main(string[] args)
    {
        // Create a collection of integer ranges.
        IntegerRangeCollection ranges = new IntegerRangeCollection();
        ranges.Add(new IntegerRange(1, 10));
        ranges.Add(new IntegerRange(11, 20));
        ranges.Add(new IntegerRange(21, 30));

        // Create a formatter for integer ranges.
        IntegerRangeFormatter formatter = new IntegerRangeFormatter("{{0}} to {{1}}");

        // Format the collection of integer ranges using the formatter.
        string formattedRanges = ranges.Format(formatter);

        // Display the formatted ranges.
        Console.WriteLine(formattedRanges);
    }
}
```

Explanation:

1. We define a delegate type called `NumberFormatter` which represents a function that takes an integer as input and returns a string as output.

2. We define a class called `IntegerRange` which represents a range of integers specified by a starting and ending value. It has a method called `Format` which uses a `NumberFormatter` delegate to format the range as a string.

3. We define a class called `IntegerRangeCollection` which represents a collection of `IntegerRange` objects. It has a method called `Format` which uses a `NumberFormatter` delegate to format the entire collection as a string.

4. We define a class called `IntegerRangeFormatter` which implements the `NumberFormatter` delegate. It provides a way to format an `IntegerRange` object using a specified format string.

5. In the `Main` method, we create an `IntegerRangeCollection` object and add three `IntegerRange` objects to it.

6. We create an `IntegerRangeFormatter` object and specify a format string.

7. We use the `Format` method of the `IntegerRangeCollection` object to format the collection of ranges using the `IntegerRangeFormatter` object.

8. We display the formatted ranges using `Console.WriteLine`.

This code demonstrates the use of delegates and lambda expressions to format data in a flexible manner. It also shows how to define custom classes to represent complex data structures and operations.