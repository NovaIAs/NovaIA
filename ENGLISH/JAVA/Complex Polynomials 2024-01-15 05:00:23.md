```java
import java.util.*;
import java.math.*;
import java.io.*;

public class ComplexCode {

    // Define a class to represent a complex number
    private static class Complex {
        private double real;
        private double imaginary;

        public Complex(double real, double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        public Complex add(Complex other) {
            return new Complex(this.real + other.real, this.imaginary + other.imaginary);
        }

        public Complex subtract(Complex other) {
            return new Complex(this.real - other.real, this.imaginary - other.imaginary);
        }

        public Complex multiply(Complex other) {
            return new Complex(
                    this.real * other.real - this.imaginary * other.imaginary,
                    this.real * other.imaginary + this.imaginary * other.real
            );
        }

        public Complex divide(Complex other) {
            double denominator = other.real * other.real + other.imaginary * other.imaginary;
            return new Complex(
                    (this.real * other.real + this.imaginary * other.imaginary) / denominator,
                    (this.imaginary * other.real - this.real * other.imaginary) / denominator
            );
        }

        public String toString() {
            return String.format("(%.2f, %.2f)", this.real, this.imaginary);
        }
    }

    // Define a class to represent a polynomial
    private static class Polynomial {
        private List<Complex> coefficients;

        public Polynomial(List<Complex> coefficients) {
            this.coefficients = coefficients;
        }

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

        public Polynomial subtract(Polynomial other) {
            List<Complex> newCoefficients = new ArrayList<>();
            int maxDegree = Math.max(this.coefficients.size(), other.coefficients.size());
            for (int i = 0; i < maxDegree; i++) {
                Complex thisCoefficient = this.coefficients.get(