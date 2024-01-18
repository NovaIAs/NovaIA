// Objective-C code to calculate the determinant of a matrix

// Function to calculate the determinant of a matrix using recursion
double determinant(int matrix[][], int n) {
  // Base case: 2x2 matrix
  if (n == 2) {
    return (matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0]);
  }

  // Calculate the determinant for a larger matrix
  double det = 0;
  for (int i = 0; i < n; i++) {
    // Create a submatrix without the i-th row and the first column
    int submatrix[n - 1][n - 1];
    for (int j = 0; j < n - 1; j++) {
      for (int k = 0; k < n - 1; k++) {
        submatrix[j][k] = matrix[j + 1][k + (i == 0 ? 0 : 1)];
      }
    }

    // Calculate the determinant of the submatrix
    double submatrix_det = determinant(submatrix, n - 1);

    // Add/subtract the determinant of the submatrix to the determinant
    // based on the sign of the element in the first row and i-th column
    det += (i % 2 == 0 ? 1 : -1) * matrix[0][i] * submatrix_det;
  }

  return det;
}

// Main function to test the determinant function
int main() {
  // Define a matrix
  int matrix[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};

  // Calculate and print the determinant of the matrix
  double det = determinant(matrix, 3);
  printf("Determinant of the matrix: %lf\n", det);

  return 0;
}


// Explanation:

The provided code is written in Objective-C and aims to compute the determinant of a square matrix. It employs recursive and versatile logic. Here's a step-by-step explanation:

1.	Function determinant:
•	This function takes a square matrix matrix and its dimension n as arguments and returns the determinant of the matrix.
•	It contains a base case for the 2x2 matrix (n == 2) and calculates the determinant using a simple formula.
•	For larger matrices, it recursively computes the determinant.

2.	Calculating Determinant for Larger Matrices:
•	For matrices with n > 2, the function employs a recursion technique.
•	In each recursive call, it creates a submatrix by excluding the first row and the column corresponding to the current element in the first row.
•	It calculates the determinant of the submatrix using a recursive call.
•	The determinant of the main matrix is calculated by combining the determinant of the submatrix and the element in the first row of the main matrix based on its sign.

3.	Main Function:
•	The main function is where the program execution starts.
•	It defines a 3x3 matrix and assigns values to it.
•	It calls the determinant function to compute the determinant of the matrix and prints the result.

This code provides a versatile and efficient solution for calculating the determinant of a square matrix. Its recursive approach allows it to handle matrices of any size, and its adaptability makes it reusable for various applications.