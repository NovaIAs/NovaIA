```cool
class Complex {
  private:
    int real;
    int imaginary;

  public:
    // Constructor
    Complex(int real, int imaginary) {
      this->real = real;
      this->imaginary = imaginary;
    }

    // Getter methods
    int getReal() {
      return this->real;
    }

    int getImaginary() {
      return this->imaginary;
    }

    // Setter methods
    void setReal(int real) {
      this->real = real;
    }

    void setImaginary(int imaginary) {
      this->imaginary = imaginary;
    }

    // Addition operator
    Complex operator+(const Complex& other) {
      Complex result(this->real + other.real, this->imaginary + other.imaginary);
      return result;
    }

    // Subtraction operator
    Complex operator-(const Complex& other) {
      Complex result(this->real - other.real, this->imaginary - other.imaginary);
      return result;
    }

    // Multiplication operator
    Complex operator*(const Complex& other) {
      int realPart = (this->real * other.real) - (this->imaginary * other.imaginary);
      int imaginaryPart = (this->real * other.imaginary) + (this->imaginary * other.real);
      Complex result(realPart, imaginaryPart);
      return result;
    }

    // Division operator
    Complex operator/(const Complex& other) {
      int denominator = (other.real * other.real) + (other.imaginary * other.imaginary);
      Complex result(((this->real * other.real) + (this->imaginary * other.imaginary)) / denominator,
                     ((this->imaginary * other.real) - (this->real * other.imaginary)) / denominator);
      return result;
    }

    // Print method
    void print() {
      cout << this->real << " + " << this->imaginary << "i" << endl;
    }
};

int main() {
  // Create two complex numbers
  Complex c1(3, 4);
  Complex c2(5, -2);

  // Print the complex numbers
  cout << "c1 = ";
  c1.print();

  cout << "c2 = ";
  c2.print();

  // Perform arithmetic operations
  Complex c3 = c1 + c2;
  Complex c4 = c1 - c2;
  Complex c5 = c1 * c2;
  Complex c6 = c1 / c2;

  // Print the results
  cout << "c1 + c2 = ";
  c3.print();

  cout << "c1 - c2 = ";
  c4.print();

  cout << "c1 * c2 = ";
  c5.print();

  cout << "c1 / c2 = ";
  c6.print();

  return 0;
}
```

Explanation:

1. Class Declaration:

   ```cool
   class Complex {
     private:
       int real;
       int imaginary;

     public:
       // ...
   };
   ```

   This declares a class called `Complex` that encapsulates the real and imaginary parts of complex numbers.

2. Constructor:

   ```cool
   Complex(int real, int imaginary) {
     this->real = real;
     this->imaginary = imaginary;
   }
   ```

   This is the constructor for the `Complex` class. It takes two integers as arguments and initializes the real and imaginary parts of the complex number.

3. Getter and Setter Methods:

   ```cool
   int getReal() {
     return this->real;
   }

   int getImaginary() {
     return this->imaginary;
   }

   void setReal(int real) {
     this->real = real;
   }

   void setImaginary(int imaginary) {
     this->imaginary = imaginary;
   }
   ```

   These are getter and setter methods that allow you to access and modify the real and imaginary parts of a complex number.

4. Arithmetic Operators:

   ```cool
   Complex operator+(const Complex& other) {
     // ...
   }

   Complex operator-(const Complex& other) {
     // ...
   }

   Complex operator*(const Complex& other) {
     // ...
   }

   Complex operator/(const Complex& other) {
     // ...
   }
   ```

   These are overloaded arithmetic operators that define how complex numbers can be added, subtracted, multiplied, and divided.

5. Print Method:

   ```cool
   void print() {
     cout << this->real << " + " << this->imaginary << "i" << endl;
   }
   ```

   This method prints a complex number in the format `real + imaginaryi`.

6. Main Function:

   ```cool
   int main() {
     // ...
   }
   ```

   This is the main function where the program starts executing.

7. Creating Complex Numbers:

   ```cool
   Complex c1(3, 4);
   Complex c2(5, -2);
   ```

   These lines create two complex numbers, `c1` and `c2`, with the values (3, 4) and (5, -2) respectively.

8. Performing Arithmetic Operations:

   ```cool
   Complex c3 = c1 + c2;
   Complex c4 = c1 - c2;
   Complex c5 = c1 * c2;
   Complex c6 = c1 / c2;
   ```

   These lines perform arithmetic operations on the complex numbers `c1` and `c2` and store the results in `c3`, `c4`, `c5`, and `c6`.

9. Printing the Results:

   ```cool
   cout << "c1 + c2 = ";
   c3.print();

   cout << "c1 - c2 = ";
   c4.print();

   cout << "c1 * c2 = ";
   c5.print();

   cout << "c1 / c2 = ";
   c6.print();
   ```

   These lines print the results of the arithmetic operations to the console.