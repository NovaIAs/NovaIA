```java

import java.util.*;
import java.io.*;
import java.lang.Math;

public class ComplexJavaCode {

    // This class represents a complex number with real and imaginary parts.
    private static class Complex {
        private double real;
        private double imaginary;

        // Constructor to create a complex number with the given real and imaginary parts.
        public Complex(double real, double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        // Method to add two complex numbers.
        public Complex add(Complex other) {
            return new Complex(this.real + other.real, this.imaginary + other.imaginary);
        }

        // Method to subtract two complex numbers.
        public Complex subtract(Complex other) {
            return new Complex(this.real - other.real, this.imaginary - other.imaginary);
        }

        // Method to multiply two complex numbers.
        public Complex multiply(Complex other) {
            return new Complex(this.real * other.real - this.imaginary * other.imaginary,
                    this.real * other.imaginary + this.imaginary * other.real);
        }

        // Method to divide two complex numbers.
        public Complex divide(Complex other) {
            double denominator = Math.pow(other.real, 2) + Math.pow(other.imaginary, 2);
            return new Complex((this.real * other.real + this.imaginary * other.imaginary) / denominator,
                    (this.imaginary * other.real - this.real * other.imaginary) / denominator);
        }

        // Method to find the magnitude of a complex number.
        public double magnitude() {
            return Math.sqrt(Math.pow(this.real, 2) + Math.pow(this.imaginary, 2));
        }

        // Method to find the argument of a complex number.
        public double argument() {
            return Math.atan2(this.imaginary, this.real);
        }

        // Method to find the complex conjugate of a complex number.
        public Complex conjugate() {
            return new Complex(this.real, -this.imaginary);
        }

        // Method to return a string representation of a complex number.
        @Override
        public String toString() {
            return "(" + this.real + " + " + this.imaginary + "i" + ")";
        }
    }

    // This class represents a polynomial with complex coefficients.
    private static class Polynomial {
        private List<Complex> coefficients;

        // Constructor to create a polynomial with the given coefficients.
        public Polynomial(List<Complex> coefficients) {
            this.coefficients = new ArrayList<>(coefficients);
        }

        // Method to add two polynomials.
        public Polynomial add(Polynomial other) {
            List<Complex> newCoefficients = new ArrayList<>();
            int maxDegree = Math.max(this.coefficients.size(), other.coefficients.size());
            for (int i = 0; i < maxDegree; i++) {
                Complex thisCoefficient = this.coefficients.get(i);
                Complex otherCoefficient = other.coefficients.get(i);
                newCoefficients.add(thisCoefficient.add(otherCoefficient));
            }
            return new Polynomial(newCoefficients);
        }

        // Method to subtract two polynomials.
        public Polynomial subtract(Polynomial other) {
            List<Complex> newCoefficients = new ArrayList<>();
            int maxDegree = Math.max(this.coefficients.size(), other.coefficients.size());
            for (int i = 0; i < maxDegree; i++) {
                Complex thisCoefficient = this.coefficients.get(i);
                Complex otherCoefficient = other.coefficients.get(i);
                newCoefficients.add(thisCoefficient.subtract(otherCoefficient));
            }
            return new Polynomial(newCoefficients);
        }

        // Method to multiply two polynomials.
        public Polynomial multiply(Polynomial other) {
            List<Complex> newCoefficients = new ArrayList<>();
            for (int i = 0; i < this.coefficients.size() + other.coefficients.size() - 1; i++) {
                newCoefficients.add(new Complex(0, 0));
            }
            for (int i = 0; i < this.coefficients.size(); i++) {
                for (int j = 0; j < other.coefficients.size(); j++) {
                    Complex thisCoefficient = this.coefficients.get(i);
                    Complex otherCoefficient = other.coefficients.get(j);
                    Complex productCoefficient = thisCoefficient.multiply(otherCoefficient);
                    newCoefficients.set(i + j, newCoefficients.get(i + j).add(productCoefficient));
                }
            }
            return new Polynomial(newCoefficients);
        }

        // Method to divide two polynomials.
        public Polynomial divide(Polynomial other) {
            if (other.coefficients.get(other.coefficients.size() - 1).equals(new Complex(0, 0))) {
                throw new ArithmeticException("Division by zero");
            }
            List<Complex> newCoefficients = new ArrayList<>();
            int degreeDifference = this.coefficients.size() - other.coefficients.size();
            for (int i = 0; i <= degreeDifference; i++) {
                newCoefficients.add(new Complex(0, 0));
            }
            Complex leadingCoefficientRatio = this.coefficients.get(this.coefficients.size() - 1).divide(other.coefficients.get(other.coefficients.size() - 1));
            newCoefficients.set(degreeDifference, leadingCoefficientRatio);
            for (int i = this.coefficients.size() - 2; i >= 0; i--) {
                Complex coefficient = this.coefficients.get(i);
                for (int j = other.coefficients.size() - 1; j >= 0; j--) {
                    Complex otherCoefficient = other.coefficients.get(j);
                    Complex productCoefficient = otherCoefficient.multiply(leadingCoefficientRatio);
                    coefficient = coefficient.subtract(productCoefficient);
                }
                newCoefficients.set(i - degreeDifference, coefficient);
            }
            return new Polynomial(newCoefficients);
        }

        // Method to find the roots of a polynomial using the quadratic formula.
        public List<Complex> findRoots() {
            List<Complex> roots = new ArrayList<>();
            for (int i = 0; i < this.coefficients.size(); i++) {
                Complex coefficient = this.coefficients.get(i);
                if (coefficient.equals(new Complex(0, 0))) {
                    roots.add(new Complex(0, 0));
                } else if (i == this.coefficients.size() - 1) {
                    roots.add(new Complex(-coefficient.real / coefficient.imaginary, 0));
                } else if (i == this.coefficients.size() - 2) {
                    roots.add(new Complex(-coefficient.real / (2 * coefficient.imaginary), Math.sqrt(-coefficient.real * coefficient.real / (4 * coefficient.imaginary * coefficient.imaginary))));
                    roots.add(new Complex(-coefficient.real / (2 * coefficient.imaginary), -Math.sqrt(-coefficient.real * coefficient.real / (4 * coefficient.imaginary * coefficient.imaginary))));
                } else {
                    throw new RuntimeException("Polynomial degree is too high for the quadratic formula");
                }
            }
            return roots;
        }

        // Method to find the derivative of a polynomial.
        public Polynomial derivative() {
            List<Complex> newCoefficients = new ArrayList<>();
            for (int i = 1; i < this.coefficients.size(); i++) {
                Complex coefficient = this.coefficients.get(i);
                newCoefficients.add(coefficient.multiply(new Complex(i, 0)));
            }
            return new Polynomial(newCoefficients);
        }

        // Method to find the integral of a polynomial.
        public Polynomial integral() {
            List<Complex