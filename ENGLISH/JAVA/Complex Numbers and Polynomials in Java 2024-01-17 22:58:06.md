```java
import java.util.*;
import java.io.*;
import java.math.*;

public class ComplexJavaCode {

    // Class to represent a complex number
    static class Complex {
        double real;
        double imaginary;

        Complex(double real, double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        // Add two complex numbers
        Complex add(Complex other) {
            return new Complex(this.real + other.real, this.imaginary + other.imaginary);
        }

        // Subtract two complex numbers
        Complex subtract(Complex other) {
            return new Complex(this.real - other.real, this.imaginary - other.imaginary);
        }

        // Multiply two complex numbers
        Complex multiply(Complex other) {
            double realPart = this.real * other.real - this.imaginary * other.imaginary;
            double imaginaryPart = this.real * other.imaginary + this.imaginary * other.real;
            return new Complex(realPart, imaginaryPart);
        }

        // Divide two complex numbers
        Complex divide(Complex other) {
            double denominator = other.real * other.real + other.imaginary * other.imaginary;
            double realPart = (this.real * other.real + this.imaginary * other.imaginary) / denominator;
            double imaginaryPart = (this.imaginary * other.real - this.real * other.imaginary) / denominator;
            return new Complex(realPart, imaginaryPart);
        }

        // Find the magnitude of a complex number
        double magnitude() {
            return Math.sqrt(this.real * this.real + this.imaginary * this.imaginary);
        }

        // Find the argument of a complex number
        double argument() {
            return Math.atan2(this.imaginary, this.real);
        }

        // Convert a complex number to a string
        @Override
        public String toString() {
            return String.format("(%.2f, %.2f)", this.real, this.imaginary);
        }
    }

    // Class to represent a polynomial
    static class Polynomial {
        double[] coefficients;

        Polynomial(double[] coefficients) {
            this.coefficients = coefficients;
        }

        // Evaluate the polynomial at a given value
        double evaluate(double x) {
            double result = 0;
            for (int i = 0; i < this.coefficients.length; i++) {
                result += this.coefficients[i] * Math.pow(x, i);
            }
            return result;
        }

        // Add two polynomials
        Polynomial add(Polynomial other) {
            int maxDegree = Math.max(this.coefficients.length, other.coefficients.length);
            double[] newCoefficients = new double[maxDegree];
            for (int i = 0; i < maxDegree; i++) {
                if (i < this.coefficients.length) {
                    newCoefficients[i] += this.coefficients[i];
                }
                if (i < other.coefficients.length) {
                    newCoefficients[i] += other.coefficients[i];
                }
            }
            return new Polynomial(newCoefficients);
        }

        // Subtract two polynomials
        Polynomial subtract(Polynomial other) {
            int maxDegree = Math.max(this.coefficients.length, other.coefficients.length);
            double[] newCoefficients = new double[maxDegree];
            for (int i = 0; i < maxDegree; i++) {
                if (i < this.coefficients.length) {
                    newCoefficients[i] += this.coefficients[i];
                }
                if (i < other.coefficients.length) {
                    newCoefficients[i] -= other.coefficients[i];
                }
            }
            return new Polynomial(newCoefficients);
        }

        // Multiply two polynomials
        Polynomial multiply(Polynomial other) {
            int degree = this.coefficients.length + other.coefficients.length - 1;
            double[] newCoefficients = new double[degree];
            for (int i = 0; i < this.coefficients.length; i++) {
                for (int j = 0; j < other.coefficients.length; j++) {
                    newCoefficients[i + j] += this.coefficients[i] * other.coefficients[j];
                }
            }
            return new Polynomial(newCoefficients);
        }

        // Differentiate the polynomial
        Polynomial differentiate() {
            double[] newCoefficients = new double[this.coefficients.length - 1];
            for (int i = 1; i < this.coefficients.length; i++) {
                newCoefficients[i - 1] = i * this.coefficients[i];
            }
            return new Polynomial(newCoefficients);
        }

        // Integrate the polynomial
        Polynomial integrate() {
            double[] newCoefficients = new double[this.coefficients.length + 1];
            newCoefficients[0] = 0;
            for (int i = 1; i < newCoefficients.length; i++) {
                newCoefficients[i] = this.coefficients[i - 1] / i;
            }
            return new Polynomial(newCoefficients);
        }

        // Find the roots of the polynomial
        Complex[] roots() {
            if (this.coefficients.length == 1) {
                return new Complex[] { new Complex(-this.coefficients[0], 0) };
            } else if (this.coefficients.length == 2) {
                double a = this.coefficients[1];