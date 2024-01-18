```java
import java.util.*;
import java.io.*;
import java.math.*;

public class ComplexJavaCode {

    // Define a class to represent a complex number
    public static class Complex {
        private double real;
        private double imaginary;

        public Complex(double real, double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        public double getReal() {
            return real;
        }

        public double getImaginary() {
            return imaginary;
        }

        // Define methods for addition, subtraction, multiplication, and division of complex numbers
        public Complex add(Complex other) {
            return new Complex(real + other.real, imaginary + other.imaginary);
        }

        public Complex subtract(Complex other) {
            return new Complex(real - other.real, imaginary - other.imaginary);
        }

        public Complex multiply(Complex other) {
            return new Complex(real * other.real - imaginary * other.imaginary,
                    real * other.imaginary + imaginary * other.real);
        }

        public Complex divide(Complex other) {
            double denominator = Math.pow(other.real, 2) + Math.pow(other.imaginary, 2);
            return new Complex((real * other.real + imaginary * other.imaginary) / denominator,
                    (imaginary * other.real - real * other.imaginary) / denominator);
        }

        // Define a method to calculate the magnitude of a complex number
        public double magnitude() {
            return Math.sqrt(Math.pow(real, 2) + Math.pow(imaginary, 2));
        }

        // Define a method to calculate the argument of a complex number
        public double argument() {
            return Math.atan2(imaginary, real);
        }

        // Define a method to convert a complex number to a string
        public String toString() {
            return "(" + real + " + " + imaginary + "i)";
        }
    }

    // Define a class to represent a polynomial
    public static class Polynomial {
        private List<Double> coefficients;

        public Polynomial(List<Double> coefficients) {
            this.coefficients = coefficients;
        }

        public List<Double> getCoefficients() {
            return coefficients;
        }

        // Define methods for addition, subtraction, multiplication, and division of polynomials
        public Polynomial add(Polynomial other) {
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < Math.max(coefficients.size(), other.coefficients.size()); i++) {
                double coefficient1 = i < coefficients.size() ? coefficients.get(i) : 0;
                double coefficient2 = i < other.coefficients.size() ? other.coefficients.get(i) : 0;
                newCoefficients.add(coefficient1 + coefficient2);
            }
            return new Polynomial(newCoefficients);
        }

        public Polynomial subtract(Polynomial other) {
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < Math.max(coefficients.size(), other.coefficients.size()); i++) {
                double coefficient1 = i < coefficients.size() ? coefficients.get(i) : 0;
                double coefficient2 = i < other.coefficients.size() ? other.coefficients.get(i) : 0;
                newCoefficients.add(coefficient1 - coefficient2);
            }
            return new Polynomial(newCoefficients);
        }

        public Polynomial multiply(Polynomial other) {
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < coefficients.size() + other.coefficients.size() - 1; i++) {
                newCoefficients.add(0.0);
            }
            for (int i = 0; i < coefficients.size(); i++) {
                for (int j = 0; j < other.coefficients.size(); j++) {
                    newCoefficients.set(i + j, newCoefficients.get(i + j) + coefficients.get(i) * other.coefficients.get(j));
                }
            }
            return new Polynomial(newCoefficients);
        }

        public Polynomial divide(Polynomial other) {
            if (other.coefficients.size() > coefficients.size()) {
                return new Polynomial(Arrays.asList(0.0));
            }
            List<Double> newCoefficients = new ArrayList<>();
            Polynomial quotient = new Polynomial(newCoefficients);
            Polynomial remainder = new Polynomial(coefficients);
            while (remainder.coefficients.size() >= other.coefficients.size()) {
                double leadingCoefficientRatio = remainder.coefficients.get(remainder.coefficients.size() - 1) / other.coefficients.get(other.coefficients.size() - 1);
                List<Double> quotientCoefficients = new ArrayList<>();
                for (int i = 0; i < other.coefficients.size(); i++) {
                    quotientCoefficients.add(leadingCoefficientRatio * other.coefficients.get(i));
                }
                Polynomial quotientTerm = new Polynomial(quotientCoefficients);
                quotient = quotient.add(quotientTerm);
                remainder = remainder.subtract(quotientTerm.multiply(other));
            }
            return quotient;
        }

        // Define a method to evaluate a polynomial at a given value of x
        public double evaluate(double x) {
            double result = 0.0;
            for (int i = 0; i < coefficients.size(); i++) {
                result += coefficients.get(i) * Math.pow(x, i);
            }
            return result;
        }

        // Define a method to find the roots of a polynomial using the