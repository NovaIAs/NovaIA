```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplexCSharpCode
{
    // This class represents a complex number, with real and imaginary parts.
    class ComplexNumber
    {
        private double _real;
        private double _imaginary;

        // Constructors
        public ComplexNumber()
        {
            _real = 0;
            _imaginary = 0;
        }

        public ComplexNumber(double real, double imaginary)
        {
            _real = real;
            _imaginary = imaginary;
        }

        // Properties
        public double Real
        {
            get { return _real; }
            set { _real = value; }
        }

        public double Imaginary
        {
            get { return _imaginary; }
            set { _imaginary = value; }
        }

        // Methods
        public ComplexNumber Add(ComplexNumber other)
        {
            return new ComplexNumber(_real + other._real, _imaginary + other._imaginary);
        }

        public ComplexNumber Subtract(ComplexNumber other)
        {
            return new ComplexNumber(_real - other._real, _imaginary - other._imaginary);
        }

        public ComplexNumber Multiply(ComplexNumber other)
        {
            return new ComplexNumber(_real * other._real - _imaginary * other._imaginary,
                _real * other._imaginary + _imaginary * other._real);
        }

        public ComplexNumber Divide(ComplexNumber other)
        {
            double denominator = other._real * other._real + other._imaginary * other._imaginary;
            return new ComplexNumber((_real * other._real + _imaginary * other._imaginary) / denominator,
                (_imaginary * other._real - _real * other._imaginary) / denominator);
        }

        public override string ToString()
        {
            return $"{_real} + {_imaginary}i";
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Create two complex numbers
            ComplexNumber z1 = new ComplexNumber(3, 4);
            ComplexNumber z2 = new ComplexNumber(5, -2);

            // Perform operations on the complex numbers
            ComplexNumber sum = z1.Add(z2);
            ComplexNumber difference = z1.Subtract(z2);
            ComplexNumber product = z1.Multiply(z2);
            ComplexNumber quotient = z1.Divide(z2);

            // Display the results
            Console.WriteLine($"Sum: {sum}");
            Console.WriteLine($"Difference: {difference}");
            Console.WriteLine($"Product: {product}");
            Console.WriteLine($"Quotient: {quotient}");
        }
    }
}
```

Explanation:

* The `ComplexNumber` class represents a complex number, with real and imaginary parts. It has two private fields, `_real` and `_imaginary`, to store the real and imaginary parts of the complex number, respectively.

* The `ComplexNumber` class has two constructors: a default constructor that initializes both `_real` and `_imaginary` to 0, and a constructor that takes two double parameters, `real` and `imaginary`, and initializes `_real` and `_imaginary` to these values.

* The `ComplexNumber` class has four methods: `Add`, `Subtract`, `Multiply`, and `Divide`, which perform the corresponding operations on two complex numbers.

* The `ToString` method overrides the default `ToString` method of the `object` class, and returns a string representation of the complex number in the format "real + imaginaryi".

* The `Program` class contains the `Main` method, which is the entry point of the program.

* In the `Main` method, two complex numbers, `z1` and `z2`, are created using the `ComplexNumber` class.

* The `Main` method then performs the four basic arithmetic operations on the two complex numbers using the `Add`, `Subtract`, `Multiply`, and `Divide` methods of the `ComplexNumber` class.

* The results of the operations are displayed on the console using the `WriteLine` method of the `Console` class.