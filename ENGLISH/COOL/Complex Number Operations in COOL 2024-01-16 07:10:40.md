```cool
class ComplexNumber {
  private Float real;
  private Float imaginary;

  public ComplexNumber(Float real, Float imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  public Float getReal() {
    return real;
  }

  public Float getImaginary() {
    return imaginary;
  }

  public ComplexNumber add(ComplexNumber other) {
    return new ComplexNumber(real + other.real, imaginary + other.imaginary);
  }

  public ComplexNumber subtract(ComplexNumber other) {
    return new ComplexNumber(real - other.real, imaginary - other.imaginary);
  }

  public ComplexNumber multiply(ComplexNumber other) {
    return new ComplexNumber(real * other.real - imaginary * other.imaginary,
                            real * other.imaginary + imaginary * other.real);
  }

  public ComplexNumber divide(ComplexNumber other) {
    Float denominator = other.real * other.real + other.imaginary * other.imaginary;
    return new ComplexNumber((real * other.real + imaginary * other.imaginary) / denominator,
                            (imaginary * other.real - real * other.imaginary) / denominator);
  }

  public String toString() {
    return real + " + " + imaginary + "i";
  }
}

class Main {
  public static void main(String[] args) {
    ComplexNumber c1 = new ComplexNumber(1.0, 2.0);
    ComplexNumber c2 = new ComplexNumber(3.0, 4.0);

    System.out.println("c1 = " + c1);
    System.out.println("c2 = " + c2);

    System.out.println("c1 + c2 = " + c1.add(c2));
    System.out.println("c1 - c2 = " + c1.subtract(c2));
    System.out.println("c1 * c2 = " + c1.multiply(c2));
    System.out.println("c1 / c2 = " + c1.divide(c2));
  }
}
```

Explanation:

1. We define a class called `ComplexNumber` to represent complex numbers. This class has three instance variables: `real` and `imaginary` to store the real and imaginary parts of the complex number, and a `constructor` to initialize these variables when a new `ComplexNumber` object is created.

2. The class also defines several methods for performing common operations on complex numbers, such as `add`, `subtract`, `multiply`, and `divide`. These methods take another `ComplexNumber` object as an argument and return a new `ComplexNumber` object representing the result of the operation.

3. We also define a `toString` method, which returns a string representation of the complex number in the format "`real + imaginaryi`".

4. In the `Main` class, we create two `ComplexNumber` objects, `c1` and `c2`, and then use the methods defined in the `ComplexNumber` class to perform various operations on these objects. The results of these operations are printed to the console.

This code demonstrates how to define and use a class in COOL to represent and manipulate complex numbers. It also shows how to define methods for performing common operations on objects of that class.