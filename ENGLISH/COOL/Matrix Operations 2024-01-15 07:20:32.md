```cool
class Matrix {
    // Class attributes
    private Int rows;
    private Int cols;
    private Int[][] data;

    // Constructor
    public Matrix(Int rows, Int cols) {
        this.rows = rows;
        this.cols = cols;
        this.data = new Int[rows][cols];
    }

    // Accessors
    public Int getRows() {
        return this.rows;
    }

    public Int getCols() {
        return this.cols;
    }

    public Int get(Int row, Int col) {
        return this.data[row][col];
    }

    // Mutators
    public void set(Int row, Int col, Int value) {
        this.data[row][col] = value;
    }

    // Methods
    public Matrix add(Matrix other) {
        if (this.rows != other.rows || this.cols != other.cols) {
            throw new IllegalArgumentException("Matrices must have the same dimensions to be added.");
        }

        Matrix result = new Matrix(this.rows, this.cols);
        for (Int i = 0; i < this.rows; i++) {
            for (Int j = 0; j < this.cols; j++) {
                result.set(i, j, this.get(i, j) + other.get(i, j));
            }
        }

        return result;
    }

    public Matrix subtract(Matrix other) {
        if (this.rows != other.rows || this.cols != other.cols) {
            throw new IllegalArgumentException("Matrices must have the same dimensions to be subtracted.");
        }

        Matrix result = new Matrix(this.rows, this.cols);
        for (Int i = 0; i < this.rows; i++) {
            for (Int j = 0; j < this.cols; j++) {
                result.set(i, j, this.get(i, j) - other.get(i, j));
            }
        }

        return result;
    }

    public Matrix multiply(Matrix other) {
        if (this.cols != other.rows) {
            throw new IllegalArgumentException("Number of columns in first matrix must be equal to number of rows in second matrix.");
        }

        Matrix result = new Matrix(this.rows, other.cols);
        for (Int i = 0; i < this.rows; i++) {
            for (Int j = 0; j < other.cols; j++) {
                Int sum = 0;
                for (Int k = 0; k < this.cols; k++) {
                    sum += this.get(i, k) * other.get(k, j);
                }
                result.set(i, j, sum);
            }
        }

        return result;
    }

    public Matrix transpose() {
        Matrix result = new Matrix(this.cols, this.rows);
        for (Int i = 0; i < this.rows; i++) {
            for (Int j = 0; j < this.cols; j++) {
                result.set(j, i, this.get(i, j));
            }
        }

        return result;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Int i = 0; i < this.rows; i++) {
            for (Int j = 0; j < this.cols; j++) {
                sb.append(this.get(i, j));
                if (j < this.cols - 1) {
                    sb.append(" ");
                }
            }
            sb.append("\n");
        }

        return sb.toString();
    }
}

public class Main {
    public static void main(String[] args) {
        // Create two matrices
        Matrix m1 = new Matrix(3, 3);
        Matrix m2 = new Matrix(3, 3);

        // Set the values of the matrices
        m1.set(0, 0, 1);
        m1.set(0, 1, 2);
        m1.set(0, 2, 3);
        m1.set(1, 0, 4);
        m1.set(1, 1, 5);
        m1.set(1, 2, 6);
        m1.set(2, 0, 7);
        m1.set(2, 1, 8);
        m1.set(2, 2, 9);

        m2.set(0, 0, 10);
        m2.set(0, 1, 11);
        m2.set(0, 2, 12);
        m2.set(1, 0, 13);
        m2.set(1, 1, 14);
        m2.set(1, 2, 15);
        m2.set(2, 0, 16);
        m2.set(2, 1, 17);
        m2.set(2, 2, 18);

        // Add the matrices
        Matrix result1 = m1.add(m2);

        // Subtract the matrices
        Matrix result2 = m1.subtract(m2);

        // Multiply the matrices