```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplexCSharpCode
{
    class Program
    {
        // Define a delegate for a function that takes an integer and returns a string.
        public delegate string NumberFormatter(int number);

        // Define a class to represent a complex number.
        public class ComplexNumber
        {
            public double Real { get; set; }
            public double Imaginary { get; set; }

            public ComplexNumber(double real, double imaginary)
            {
                Real = real;
                Imaginary = imaginary;
            }

            // Define a method to add two complex numbers.
            public ComplexNumber Add(ComplexNumber other)
            {
                return new ComplexNumber(Real + other.Real, Imaginary + other.Imaginary);
            }

            // Define a method to subtract two complex numbers.
            public ComplexNumber Subtract(ComplexNumber other)
            {
                return new ComplexNumber(Real - other.Real, Imaginary - other.Imaginary);
            }

            // Define a method to multiply two complex numbers.
            public ComplexNumber Multiply(ComplexNumber other)
            {
                double real = Real * other.Real - Imaginary * other.Imaginary;
                double imaginary = Real * other.Imaginary + Imaginary * other.Real;
                return new ComplexNumber(real, imaginary);
            }

            // Define a method to divide two complex numbers.
            public ComplexNumber Divide(ComplexNumber other)
            {
                double denominator = other.Real * other.Real + other.Imaginary * other.Imaginary;
                double real = (Real * other.Real + Imaginary * other.Imaginary) / denominator;
                double imaginary = (Imaginary * other.Real - Real * other.Imaginary) / denominator;
                return new ComplexNumber(real, imaginary);
            }

            // Define a method to convert a complex number to a string.
            public override string ToString()
            {
                return $"({Real}, {Imaginary})";
            }
        }

        // Define a class to represent a polynomial.
        public class Polynomial
        {
            public List<double> Coefficients { get; set; }

            public Polynomial(params double[] coefficients)
            {
                Coefficients = new List<double>(coefficients);
            }

            // Define a method to evaluate a polynomial at a given value.
            public double Evaluate(double x)
            {
                double result = 0;
                for (int i = 0; i < Coefficients.Count; i++)
                {
                    result += Coefficients[i] * Math.Pow(x, i);
                }
                return result;
            }

            // Define a method to add two polynomials.
            public Polynomial Add(Polynomial other)
            {
                Polynomial result = new Polynomial();
                result.Coefficients = new List<double>();
                for (int i = 0; i < Math.Max(Coefficients.Count, other.Coefficients.Count); i++)
                {
                    double coefficient1 = i < Coefficients.Count ? Coefficients[i] : 0;
                    double coefficient2 = i < other.Coefficients.Count ? other.Coefficients[i] : 0;
                    result.Coefficients.Add(coefficient1 + coefficient2);
                }
                return result;
            }

            // Define a method to subtract two polynomials.
            public Polynomial Subtract(Polynomial other)
            {
                Polynomial result = new Polynomial();
                result.Coefficients = new List<double>();
                for (int i = 0; i < Math.Max(Coefficients.Count, other.Coefficients.Count); i++)
                {
                    double coefficient1 = i < Coefficients.Count ? Coefficients[i] : 0;
                    double coefficient2 = i < other.Coefficients.Count ? other.Coefficients[i] : 0;
                    result.Coefficients.Add(coefficient1 - coefficient