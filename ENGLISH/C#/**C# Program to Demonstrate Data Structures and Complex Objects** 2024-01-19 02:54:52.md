```csharp
// Import the necessary libraries.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Define the main class.
public class Program
{
    // Define the main method.
    public static void Main(string[] args)
    {
        // Create a list of integers.
        List<int> numbers = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        // Create a list of strings.
        List<string> fruits = new List<string>() { "apple", "banana", "cherry", "durian", "elderberry", "fig" };

        // Create a dictionary of strings and integers.
        Dictionary<string, int> personAges = new Dictionary<string, int>() { { "John", 25 }, { "Mary", 30 }, { "Bob", 35 } };

        // Create a complex object.
        ComplexObject complexObject = new ComplexObject()
        {
            Number = 10,
            Name = "John",
            Age = 25,
            Friends = new List<string>() { "Mary", "Bob", "Alice" }
        };

        // Print the contents of the list of integers.
        Console.WriteLine("List of integers:");
        foreach (int number in numbers)
        {
            Console.WriteLine(number);
        }

        // Print the contents of the list of strings.
        Console.WriteLine("List of strings:");
        foreach (string fruit in fruits)
        {
            Console.WriteLine(fruit);
        }

        // Print the contents of the dictionary of strings and integers.
        Console.WriteLine("Dictionary of strings and integers:");
        foreach (KeyValuePair<string, int> personAge in personAges)
        {
            Console.WriteLine("{0} is {1} years old.", personAge.Key, personAge.Value);
        }

        // Print the contents of the complex object.
        Console.WriteLine("Complex object:");
        Console.WriteLine("Number: {0}", complexObject.Number);
        Console.WriteLine("Name: {0}", complexObject.Name);
        Console.WriteLine("Age: {0}", complexObject.Age);
        Console.WriteLine("Friends:");
        foreach (string friend in complexObject.Friends)
        {
            Console.WriteLine(friend);
        }
    }
}

// Define the ComplexObject class.
public class ComplexObject
{
    public int Number { get; set; }
    public string Name { get; set; }
    public int Age { get; set; }
    public List<string> Friends { get; set; }
}
```

**Explanation:**

The provided code is a comprehensive C# program that demonstrates the use of various data structures and complex objects. It includes:

1. **Lists:** Declares and initializes two lists: one containing integers and the other containing strings.

2. **Dictionary:** Creates a dictionary that maps strings (names) to integers (ages).

3. **Complex Object:** Defines a complex `ComplexObject` class with various properties, including a number, name, age, and a list of friends.

4. **Main Method:** The main method is the entry point of the program. It performs the following tasks:

   - Creates instances of the list, dictionary, and complex object.
   - Iterates and prints the contents of the list of integers, list of strings, and dictionary of strings and integers.
   - Prints the properties of the complex object, including the number, name, age, and friends.

This code showcases the use of data structures, object-oriented programming concepts, and the main method in C#. It provides a thorough and complex example for someone looking for a more advanced code in the language.