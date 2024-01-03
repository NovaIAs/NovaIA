```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ComplexCodeExample
{
    // Define a delegate type for a function that takes an integer and returns an integer.
    public delegate int IntFunction(int n);

    class Program
    {
        static void Main(string[] args)
        {
            // Define a function that returns the square of a number.
            IntFunction squareFunction = (n) => n * n;

            // Define a function that returns the cube of a number.
            IntFunction cubeFunction = (n) => n * n * n;

            // Create a list of integers.
            List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };

            // Apply the square function to each number in the list.
            List<int> squaredNumbers = numbers.Select(squareFunction).ToList();

            // Apply the cube function to each number in the list.
            List<int> cubedNumbers = numbers.Select(cubeFunction).ToList();

            // Print the squared and cubed numbers.
            Console.WriteLine("Squared numbers:");
            foreach (int number in squaredNumbers)
            {
                Console.WriteLine(number);
            }

            Console.WriteLine("Cubed numbers:");
            foreach (int number in cubedNumbers)
            {
                Console.WriteLine(number);
            }
        }
    }
}
```

Explanation:

1. We define a delegate type called `IntFunction` which represents a function that takes an integer as input and returns an integer as output.

2. We define two functions, `squareFunction` and `cubeFunction`, which are instances of the `IntFunction` delegate. These functions take an integer as input and return its square and cube, respectively.

3. We create a list of integers called `numbers`.

4. We use the `Select()` method on the `numbers` list to apply the `squareFunction` to each number in the list, and store the results in a new list called `squaredNumbers`.

5. We use the `Select()` method on the `numbers` list again to apply the `cubeFunction` to each number in the list, and store the results in a new list called `cubedNumbers`.

6. We use the `Console.WriteLine()` method to print the squared and cubed numbers to the console.

This code demonstrates the use of lambda expressions, delegates, and LINQ (Language Integrated Query) to perform complex operations on a list of integers in a concise and readable manner.