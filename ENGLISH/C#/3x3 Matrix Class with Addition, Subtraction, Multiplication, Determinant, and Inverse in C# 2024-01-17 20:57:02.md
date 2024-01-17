```csharp
// Define a custom exception class for invalid input
public class InvalidInputException : Exception
{
    public InvalidInputException() {}
    public InvalidInputException(string message) : base(message) {}
}

// Define a class to represent a 3x3 matrix
public class Matrix3x3
{
    private double[,] elements;

    // Constructor to initialize the matrix with zeros
    public Matrix3x3()
    {
        elements = new double[3, 3];
    }

    // Constructor to initialize the matrix with given values
    public Matrix3x3(double[,] values)
    {
        if (values.GetLength(0) != 3 || values.GetLength(1) != 3)
        {
            throw new InvalidInputException("Input array must be a 3x3 matrix.");
        }
        elements = values;
    }

    // Property to access the matrix elements
    public double[,] Elements
    {
        get { return elements; }
        set
        {
            if (value.GetLength(0) != 3 || value.GetLength(1) != 3)
            {
                throw new InvalidInputException("Input array must be a 3x3 matrix.");
            }
            elements = value;
        }
    }

    // Override the ToString() method to provide a string representation of the matrix
    public override string ToString()
    {
        string output = "";
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                output += elements[i, j] + "\t";
            }
            output += "\n";
        }
        return output;
    }

    // Define a method to perform matrix addition
    public Matrix3x3 Add(Matrix3x3 other)
    {
        if (other == null)
        {
            throw new ArgumentNullException("other");
        }

        double[,] result = new double[3, 3];
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                result[i, j] = elements[i, j] + other.elements[i, j];
            }
        }
        return new Matrix3x3(result);
    }

    // Define a method to perform matrix subtraction
    public Matrix3x3 Subtract(Matrix3x3 other)
    {
        if (other == null)
        {
            throw new ArgumentNullException("other");
        }

        double[,] result = new double[3, 3];
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                result[i, j] = elements[i, j] - other.elements[i, j];
            }
        }
        return new Matrix3x3(result);
    }

    // Define a method to perform matrix multiplication
    public Matrix3x3 Multiply(Matrix3x3 other)
    {
        if (other == null)
        {
            throw new ArgumentNullException("other");
        }

        double[,] result = new double[3, 3];
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                for (int k = 0; k < 3; k++)
                {
                    result[i, j] += elements[i, k] * other.elements[k, j];
                }
            }
        }
        return new Matrix3x3(result);
    }

    // Define a method to find the determinant of the matrix
    public double Determinant()
    {
        // Calculate the determinant using the Laplace expansion along the first row
        double det = elements[0, 0] * (elements[1, 1] * elements[2, 2] - elements[1, 2] * elements[2, 1]);
        det -= elements[0, 1] * (elements[1, 0] * elements[2, 2] - elements[1, 2] * elements[2, 0]);
        det += elements[0, 2] * (elements[1, 0] * elements[2, 1] - elements[1, 1] * elements[2, 0]);
        return det;
    }

    // Define a method to check if the matrix is invertible
    public bool IsInvertible()
    {
        return Determinant() != 0;
    }

    // Define a method to find the inverse of the matrix
    public Matrix3x3 Inverse()
    {
        if (!IsInvertible())
        {
            throw new InvalidOperationException("Matrix is not invertible.");
        }

        double det = Determinant();
        double[,] result = new double[3, 3];

        // Calculate the cofactor matrix
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                result[i, j] = (elements[(i + 1) % 3, (j + 1) % 3] * elements[(i + 2) % 3, (j + 2) % 3] -
                                elements[(i + 1) % 3, (j + 2) % 3] * elements[(i + 2) % 3, (j + 1) % 3]) / det;
            }
        }

        // Transpose the cofactor matrix to get the adjoint matrix
