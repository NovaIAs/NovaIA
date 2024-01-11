```d
import std.stdio, std.algorithm, std.immutable, std.array, std.exception;

// Define a custom exception for invalid input
class InvalidInputException extends Exception {
    this() {
        super("Invalid input");
    }
}

// Define a class to represent a matrix
class Matrix {
    private int[][] data;
    private int rows, cols;

    // Constructor to create a matrix from a 2D array
    this(int[][] data) {
        this.data = data;
        this.rows = data.length;
        this.cols = data[0].length;
    }

    // Constructor to create a matrix with specified dimensions and initial value
    this(int rows, int cols, int initialValue = 0) {
        this.data = new int[rows][cols];
        this.rows = rows;
        this.cols = cols;

        // Fill the matrix with the initial value
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                this.data[i][j] = initialValue;
            }
        }
    }

    // Getter for the number of rows
    int getRows() {
        return this.rows;
    }

    // Getter for the number of columns
    int getCols() {
        return this.cols;
    }

    // Getter for a specific element in the matrix
    int get(int row, int col) {
        if (row < 0 || row >= this.rows || col < 0 || col >= this.cols) {
            throw new IndexOutOfBoundsException("Index out of bounds");
        }
        return this.data[row][col];
    }

    // Setter for a specific element in the matrix
    void set(int row, int col, int value) {
        if (row < 0 || row >= this.rows || col < 0 || col >= this.cols) {
            throw new IndexOutOfBoundsException("Index out of bounds");
        }
        this.data[row][col] = value;
    }

    // Add two matrices together
    Matrix add(Matrix other) {
        if (this.rows != other.rows || this.cols != other.cols) {
            throw new InvalidInputException("Matrices must have the same dimensions");
        }

        Matrix result = new Matrix(this.rows, this.cols);
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < this.cols; j++) {
                result.set(i, j, this.get(i, j) + other.get(i, j));
            }
        }

        return result;
    }

    // Multiply two matrices together
    Matrix multiply(Matrix other) {
        if (this.cols != other.rows) {
            throw new InvalidInputException("Matrices cannot be multiplied");
        }

        Matrix result = new Matrix(this.rows, other.cols);
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < other.cols; j++) {
                int sum = 0;
                for (int k = 0; k < this.cols; k++) {
                    sum += this.get(i, k) * other.get(k, j);
                }
                result.set(i, j, sum);
            }
        }

        return result;
    }

    // Transpose the matrix
    Matrix transpose() {
        Matrix result = new Matrix(this.cols, this.rows);
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < this.cols; j++) {
                result.set(j, i, this.get(i, j));
            }
        }

        return result;
    }

    // Print the matrix to the console
    void print() {
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < this.cols; j++) {
                writef("%d ", this.get(i, j));
            }
            writeln;
        }
    }
}

// Define a function to read a matrix from the console
Matrix readMatrix() {
    int rows, cols;
    try {
        writef("Enter the number of rows and columns: ");
        rows = readln.to!int;
        cols = readln.to!int;
    } catch (Exception e) {
        throw new InvalidInputException("Invalid input");
    }

    Matrix matrix = new Matrix(rows, cols);
    writef("Enter the elements of the matrix: ");
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            try {
                matrix.set(i, j, readln.to!int);
            } catch (Exception e) {
                throw new InvalidInputException("Invalid input");
            }
        }
    }

    return matrix;
}

// Define a function to perform matrix operations
Matrix performOperation(Matrix matrix1, Matrix matrix2, char operation) {
    switch (operation) {
        case '+':
            return matrix1.add(matrix2);
        case '*':
            return matrix1.multiply(matrix2);
        case 'T':
            return matrix1.transpose();
        default:
            throw new InvalidInputException("Invalid operation");
    }
}

void main() {
    try {
        // Read the first matrix
        writef("Enter the first matrix: ");
        Matrix matrix1 = readMatrix();

        // Read the second matrix
        writef("Enter the second matrix: ");
        Matrix matrix2 = readMatrix();

        // Read the operation to be performed
        writef("Enter the operation to be performed (+, *, T): ");
        char operation = readln.to!char;

        // Perform the operation
        Matrix result = performOperation(matrix1, matrix2, operation);

        // Print the result
        writef("Result: ");
        result.print();
    } catch (Exception e) {
        writeln(e.message);
    }
}
```

**Explanation:**

This code demonstrates a complex program in the D programming language that performs matrix operations. It includes:

1. **Matrix Class**: Defines a class called `Matrix` to represent a matrix. It provides methods to access and modify elements, perform operations like addition, multiplication, and transposition, and print the matrix.

2. **Input Handling**: The program uses the `readMatrix()` function to read a matrix from the console. It prompts the user to enter the number of rows and columns, followed by the matrix elements. It also handles invalid input and throws an exception if encountered.

3. **Operation Selection**: The user is prompted to enter the operation to be performed on the two matrices. Valid operations include addition (`+`), multiplication (`*`), and transposition (`T`).

4. **Operation Execution**: The `performOperation()` function takes two matrices and an operation character as arguments. It uses a switch statement to determine the operation and performs the appropriate operation on the matrices.

5. **Output**: The result of the operation is then printed to the console using the `print()` method of the `Matrix` class.

This code demonstrates various features of the D programming language, including classes, methods, exception handling, input/output operations, and conditional statements. It also showcases the flexibility and expressiveness of the language in handling complex data structures and operations.