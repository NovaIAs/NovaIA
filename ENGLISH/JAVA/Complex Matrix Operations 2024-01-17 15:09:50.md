```java
import java.util.*;
import java.math.*;
import java.io.*;

public class ComplexCode {

    // A class to represent a complex number
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
            return new Complex(this.real * other.real - this.imaginary * other.imaginary,
                    this.real * other.imaginary + this.imaginary * other.real);
        }

        public Complex divide(Complex other) {
            double denominator = other.real * other.real + other.imaginary * other.imaginary;
            return new Complex((this.real * other.real + this.imaginary * other.imaginary) / denominator,
                    (this.imaginary * other.real - this.real * other.imaginary) / denominator);
        }

        public double abs() {
            return Math.sqrt(this.real * this.real + this.imaginary * this.imaginary);
        }

        @Override
        public String toString() {
            return String.format("(%.2f, %.2f)", this.real, this.imaginary);
        }
    }

    // A class to represent a matrix of complex numbers
    private static class Matrix {
        private Complex[][] elements;
        private int rows;
        private int columns;

        public Matrix(int rows, int columns) {
            this.rows = rows;
            this.columns = columns;
            this.elements = new Complex[rows][columns];
        }

        public Matrix add(Matrix other) {
            if (this.rows != other.rows || this.columns != other.columns) {
                throw new IllegalArgumentException("Matrices must have the same dimensions");
            }
            Matrix result = new Matrix(this.rows, this.columns);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.elements[i][j] = this.elements[i][j].add(other.elements[i][j]);
                }
            }
            return result;
        }

        public Matrix subtract(Matrix other) {
            if (this.rows != other.rows || this.columns != other.columns) {
                throw new IllegalArgumentException("Matrices must have the same dimensions");
            }
            Matrix result = new Matrix(this.rows, this.columns);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.elements[i][j] = this.elements[i][j].subtract(other.elements[i][j]);
                }
            }
            return result;
        }

        public Matrix multiply(Matrix other) {
            if (this.columns != other.rows) {
                throw new IllegalArgumentException("Matrices cannot be multiplied");
            }
            Matrix result = new Matrix(this.rows, other.columns);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < other.columns; j++) {
                    Complex sum = new Complex(0, 0);
                    for (int k = 0; k < this.columns; k++) {
                        sum = sum.add(this.elements[i][k].multiply(other.elements[k][j]));
                    }
                    result.elements[i][j] = sum;
                }
            }
            return result;
        }

        public Matrix transpose() {
            Matrix result = new Matrix(this.columns, this.rows);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.elements[j][i] = this.elements[i][j];
                }
            }
            return result;
        }

        public Matrix inverse() {
            if (this.rows != this.columns) {
                throw new IllegalArgumentException("Matrix must be square");