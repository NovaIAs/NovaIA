```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <utility>
#include <tuple>

using namespace std;

// Defining a custom type alias for the vector of vectors of integers
using Matrix = vector<vector<int>>;

// Defining a custom type alias for the function that takes a vector of integers and returns an integer
using Function = function<int(const vector<int>&)>;

// Defining a custom type alias for the map from strings to functions
using FunctionMap = unordered_map<string, Function>;

// Function to calculate the number of elements in a given matrix
int calculateMatrixSize(const Matrix& matrix) {
  int size = 0;
  for (const auto& row : matrix) {
    size += row.size();
  }
  return size;
}

// Function to calculate the sum of all elements in a given matrix
int calculateMatrixSum(const Matrix& matrix) {
  int sum = 0;
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      sum += element;
    }
  }
  return sum;
}

// Function to calculate the average value of all elements in a given matrix
double calculateMatrixAverage(const Matrix& matrix) {
  int size = calculateMatrixSize(matrix);
  int sum = calculateMatrixSum(matrix);
  return static_cast<double>(sum) / size;
}

// Function to calculate the maximum value of all elements in a given matrix
int calculateMatrixMaximum(const Matrix& matrix) {
  int maximum = numeric_limits<int>::min();
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      maximum = max(maximum, element);
    }
  }
  return maximum;
}

// Function to calculate the minimum value of all elements in a given matrix
int calculateMatrixMinimum(const Matrix& matrix) {
  int minimum = numeric_limits<int>::max();
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      minimum = min(minimum, element);
    }
  }
  return minimum;
}

// Function to calculate the range of values in a given matrix (maximum - minimum)
int calculateMatrixRange(const Matrix& matrix) {
  return calculateMatrixMaximum(matrix) - calculateMatrixMinimum(matrix);
}

// Function to calculate the median value of all elements in a given matrix
double calculateMatrixMedian(const Matrix& matrix) {
  // Flatten the matrix into a vector of integers
  vector<int> flattenedMatrix;
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      flattenedMatrix.push_back(element);
    }
  }

  // Sort the flattened matrix in ascending order
  sort(flattenedMatrix.begin(), flattenedMatrix.end());

  // Calculate the median value
  size_t size = flattenedMatrix.size();
  if (size % 2 == 0) {
    // Even number of elements
    return (flattenedMatrix[size / 2 - 1] + flattenedMatrix[size / 2]) / 2.0;
  } else {
    // Odd number of elements
    return flattenedMatrix[size / 2];
  }
}

// Function to calculate the standard deviation of all elements in a given matrix
double calculateMatrixStandardDeviation(const Matrix& matrix) {
  // Calculate the mean of the matrix
  double mean = calculateMatrixAverage(matrix);

  // Calculate the sum of squared differences from the mean
  double sumOfSquaredDifferences = 0;
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      sumOfSquaredDifferences += pow(element - mean, 2);
    }
  }

  // Calculate the variance
  double variance = sumOfSquaredDifferences / calculateMatrixSize(matrix);

  // Calculate the standard deviation
  return sqrt(variance);
}

// Function to print the elements of a given matrix in a tabular format
void printMatrix(const Matrix& matrix) {
  for (const auto& row : matrix) {
    for (const auto& element : row) {
      cout << setw(4) << element;
    }
    cout << endl;
  }
}

// Function to create a function map from a list of strings and functions
FunctionMap createFunctionMap(const vector<string>& functions, const vector<Function>& implementations) {
  FunctionMap functionMap;
  for (size_t i = 0; i < functions.size(); ++i) {
    functionMap[functions[i]] = implementations[i];
  }
  return functionMap;
}

// Function to apply a given function to each row of a given matrix and return a new matrix
Matrix applyFunctionToRows(const Matrix& matrix, const Function& function) {
  Matrix result(matrix.size());
  for (size_t i = 0; i < matrix.size(); ++i) {
    result[i] = function(matrix[i]);
  }
  return result;
}

// Function to apply a given function to each column of a given matrix and return a new matrix
Matrix applyFunctionToColumns(const Matrix& matrix, const Function& function) {
  Matrix result(matrix[0].size(), vector<int>(matrix.size()));
  for (size_t i = 0; i < matrix.size(); ++i) {
    for (size_t j = 0; j < matrix[0].size(); ++j) {
      result[j][i] = function({matrix[i][j]});
    }
  }
  return result;
}

// Function to calculate the transpose of a given matrix
Matrix calculateTranspose(const Matrix& matrix) {
  Matrix transpose(matrix[0].size(), vector<int>(matrix.size()));
  for (size_t i = 0; i <