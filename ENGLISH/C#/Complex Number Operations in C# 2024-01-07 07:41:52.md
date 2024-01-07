```c#
// Example of a complex and differentiated code in C#

// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Define a custom data structure
public struct ComplexNumber
{
    public double Real;
    public double Imaginary;

    public ComplexNumber(double real, double imaginary)
    {
        Real = real;
        Imaginary = imaginary;
    }

    public static ComplexNumber operator +(ComplexNumber a, ComplexNumber b)
    {
        return new ComplexNumber(a.Real + b.Real, a.Imaginary + b.Imaginary);
    }

    public static ComplexNumber operator -(ComplexNumber a, ComplexNumber b)
    {
        return new ComplexNumber(a.Real - b.Real, a.Imaginary - b.Imaginary);
    }

    public static ComplexNumber operator *(ComplexNumber a, ComplexNumber b)
    {
        return new ComplexNumber(a.Real * b.Real - a.Imaginary * b.Imaginary, a.Real * b.Imaginary + a.Imaginary * b.Real);
    }

    public static ComplexNumber operator /(ComplexNumber a, ComplexNumber b)
    {
        double denominator = b.Real * b.Real + b.Imaginary * b.Imaginary;
        return new ComplexNumber((a.Real * b.Real + a.Imaginary * b.Imaginary) / denominator, (a.Imaginary * b.Real - a.Real * b.Imaginary) / denominator);
    }

    public override string ToString()
    {
        return $"{Real} + {Imaginary}i";
    }
}

// Define a custom class
public class ComplexCalculator
{
    public static ComplexNumber Add(ComplexNumber a, ComplexNumber b)
    {
        return a + b;
    }

    public static ComplexNumber Subtract(ComplexNumber a, ComplexNumber b)
    {
        return a - b;
    }

    public static ComplexNumber Multiply(ComplexNumber a, ComplexNumber b)
    {
        return a * b;
    }

    public static ComplexNumber Divide(ComplexNumber a, ComplexNumber b)
    {
        return a / b;
    }

    public static ComplexNumber Power(ComplexNumber a, int exponent)
    {
        if (exponent == 0)
        {
            return new ComplexNumber(1, 0);
        }
        else if (exponent > 0)
        {
            ComplexNumber result = a;
            for (int i = 1; i < exponent; i++)
            {
                result *= a;
            }
            return result;
        }
        else
        {
            return Power(a, -exponent);
        }
    }
}

// Driver code
class Program
{
    static void Main(string[] args)
    {
        // Create two complex numbers
        ComplexNumber a = new ComplexNumber(3, 4);
        ComplexNumber b = new ComplexNumber(5, -2);

        // Print the complex numbers
        Console.WriteLine($"a = {a}");
        Console.WriteLine($"b = {b}");

        // Perform operations on the complex numbers
        ComplexNumber sum = ComplexCalculator.Add(a, b);
        ComplexNumber difference = ComplexCalculator.Subtract(a, b);
        ComplexNumber product = ComplexCalculator.Multiply(a, b);
        ComplexNumber quotient = ComplexCalculator.Divide(a, b);
        ComplexNumber power = ComplexCalculator.Power(a, 3);

        // Print the results
        Console.WriteLine($"a + b = {sum}");
        Console.WriteLine($"a - b = {difference}");
        Console.WriteLine($"a * b = {product}");
        Console.WriteLine($"a / b = {quotient}");
        Console.WriteLine($"a^3 = {power}");
    }
}
```

Explanation:

1. We define a custom data structure called `ComplexNumber` to represent complex numbers. It has two fields: `Real` and `Imaginary`. It also defines operators for addition, subtraction, multiplication, and division of complex numbers, as well as a `ToString()` method to print the complex number in the format `a + bi`.

2. We define a class called `ComplexCalculator` that contains static methods for performing operations on complex numbers. These methods include `Add()`, `Subtract()`, `Multiply()`, `Divide()`, and `Power()`. The `Power()` method calculates the complex number raised to a given exponent using a loop.

3. In the `Main()` method, we create two complex numbers `a` and `b`. We then use the methods in the `ComplexCalculator` class to perform various operations on these complex numbers and print the results.

This code demonstrates the use of custom data structures and classes to represent and manipulate complex numbers in C#. It also showcases the use of operator overloading and method overloading to simplify the code and make it more readable.