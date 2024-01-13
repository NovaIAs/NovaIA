```java
import java.util.*;
import java.io.*;
import java.math.*;

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

        public String toString() {
            return String.format("(%f, %f)", this.real, this.imaginary);
        }
    }

    // A class to represent a matrix
    private static class Matrix {
        private int rows;
        private int columns;
        private double[][] data;

        public Matrix(int rows, int columns) {
            this.rows = rows;
            this.columns = columns;
            this.data = new double[rows][columns];
        }

        public void set(int row, int column, double value) {
            this.data[row][column] = value;
        }

        public double get(int row, int column) {
            return this.data[row][column];
        }

        public Matrix add(Matrix other) {
            if (this.rows != other.rows || this.columns != other.columns) {
                throw new IllegalArgumentException("Matrices must have the same dimensions");
            }
            Matrix result = new Matrix(this.rows, this.columns);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.set(i, j, this.get(i, j) + other.get(i, j));
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
                    result.set(i, j, this.get(i, j) - other.get(i, j));
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
                    double sum = 0;
                    for (int k = 0; k < this.columns; k++) {
                        sum += this.get(i, k) * other.get(k, j);
                    }
                    result.set(i, j, sum);
                }
            }
            return result;
        }

        public Matrix transpose() {
            Matrix result = new Matrix(this.columns, this.rows);
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.set(j, i, this.get(i, j));
                }
            }
            return result;
        }

        public Matrix inverse() {
            if (this.rows != this.columns) {
                throw new IllegalArgumentException("Matrix must be square to have an inverse");
            }
            Matrix result = new Matrix(this.rows, this.columns);
            double determinant = this.determinant();
            if (determinant == 0) {
                throw new IllegalArgumentException("Matrix is not invertible");
            }
            for (int i = 0; i < this.rows; i++) {
                for (int j = 0; j < this.columns; j++) {
                    result.set(i, j, this.cofactor(i, j) / determinant);
                }
            }
            return result;
        }

        public double determinant() {
            if (this.rows != this.columns) {
                throw new IllegalArgumentException("Matrix must be square to have a determinant");
            }
            if (this.rows == 1) {
                return this.get(0, 0);
            }
            double determinant = 0;
            for (int i = 0; i < this.rows; i++) {
                determinant += this.get(i, 0) * this.cofactor(i, 0);
            }
            return determinant;
        }

        public double cofactor(int row, int column) {
            Matrix submatrix = new Matrix(this.rows - 1, this.columns - 1);
            int subrow = 0;
            for (int i = 0; i < this.rows; i++) {
                if (i == row) {
                    continue;
                }
                int subcolumn = 0;
                for (int j = 0; j < this.columns; j++) {
                    if (j == column) {
                        continue;
                    }
                    submatrix.set(subrow, subcolumn, this.get(i, j));
                    subcolumn++;
                }
                subrow++;
            }
            return (row + column) % 2 == 0 ? submatrix.determinant() : -submatrix.determinant();
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < this.rows; i++) {
                sb.append("[");
                for (int j = 0; j < this.columns; j++) {
                    sb.append(String.format("%f,", this.get(i, j)));
                }
                sb.deleteCharAt(sb.length() - 1);
                sb.append("],");
            }
            sb.deleteCharAt(sb.length() - 1);
            sb.append("]");
            return sb.toString();
        }
    }

    // A class to represent a polynomial
    private static class Polynomial {
        private List<Double> coefficients;

        public Polynomial(List<Double> coefficients) {
            this.coefficients = coefficients;
        }

        public Polynomial add(Polynomial other) {
            int maxDegree = Math.max(this.coefficients.size(), other.coefficients.size());
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < maxDegree; i++) {
                double thisCoefficient = this.coefficients.size() > i ? this.coefficients.get(i) : 0;
                double otherCoefficient = other.coefficients.size() > i ? other.coefficients.get(i) : 0;
                newCoefficients.add(thisCoefficient + otherCoefficient);
            }
            return new Polynomial(newCoefficients);
        }

        public Polynomial subtract(Polynomial other) {
            int maxDegree = Math.max(this.coefficients.size(), other.coefficients.size());
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < maxDegree; i++) {
                double thisCoefficient = this.coefficients.size() > i ? this.coefficients.get(i) : 0;
                double otherCoefficient = other.coefficients.size() > i ? other.coefficients.get(i) : 0;
                newCoefficients.add(thisCoefficient - otherCoefficient);
            }
            return new Polynomial(newCoefficients);
        }

        public Polynomial multiply(Polynomial other) {
            List<Double> newCoefficients = new ArrayList<>();
            for (int i = 0; i < this.coefficients.size() + other.coefficients.size() - 1; i++) {
