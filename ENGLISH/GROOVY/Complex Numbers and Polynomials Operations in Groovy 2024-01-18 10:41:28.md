```groovy
// Define a class to represent a complex number.
class ComplexNumber {
    private double real
    private double imaginary

    // Constructor to initialize a complex number.
    ComplexNumber(double real, double imaginary) {
        this.real = real
        this.imaginary = imaginary
    }

    // Getter method for the real part.
    double getReal() {
        return real
    }

    // Getter method for the imaginary part.
    double getImaginary() {
        return imaginary
    }

    // Method to add two complex numbers.
    ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(real + other.real, imaginary + other.imaginary)
    }

    // Method to subtract two complex numbers.
    ComplexNumber subtract(ComplexNumber other) {
        return new ComplexNumber(real - other.real, imaginary - other.imaginary)
    }

    // Method to multiply two complex numbers.
    ComplexNumber multiply(ComplexNumber other) {
        return new ComplexNumber(real * other.real - imaginary * other.imaginary,
                                    real * other.imaginary + imaginary * other.real)
    }

    // Method to divide two complex numbers.
    ComplexNumber divide(ComplexNumber other) {
        double denominator = other.real * other.real + other.imaginary * other.imaginary
        return new ComplexNumber((real * other.real + imaginary * other.imaginary) / denominator,
                                    (imaginary * other.real - real * other.imaginary) / denominator)
    }

    // Method to calculate the absolute value of a complex number.
    double absoluteValue() {
        return Math.sqrt(real * real + imaginary * imaginary)
    }

    // Method to calculate the argument of a complex number.
    double argument() {
        return Math.atan2(imaginary, real)
    }

    // Method to print a complex number in the format (a + bi).
    @Override
    String toString() {
        return "($real + ${imaginary}i)"
    }
}

// Define a class to represent a polynomial.
class Polynomial {
    private List<Double> coefficients

    // Constructor to initialize a polynomial.
    Polynomial(List<Double> coefficients) {
        this.coefficients = coefficients
    }

    // Getter method for the coefficients.
    List<Double> getCoefficients() {
        return coefficients
    }

    // Method to evaluate a polynomial at a given value of x.
    double evaluate(double x) {
        double result = 0
        for (int i = 0; i < coefficients.size(); i++) {
            result += coefficients[i] * Math.pow(x, i)
        }
        return result
    }

    // Method to differentiate a polynomial.
    Polynomial differentiate() {
        List<Double> newCoefficients = []
        for (int i = 1; i < coefficients.size(); i++) {
            newCoefficients.add(i * coefficients[i])
        }
        return new Polynomial(newCoefficients)
    }

    // Method to integrate a polynomial.
    Polynomial integrate() {
        List<Double> newCoefficients = []
        for (int i = 0; i < coefficients.size(); i++) {
            newCoefficients.add(coefficients[i] / (i + 1))
        }
        return new Polynomial(newCoefficients)
    }

    // Method to add two polynomials.
    Polynomial add(Polynomial other) {
        List<Double> newCoefficients = []
        for (int i = 0; i < Math.max(coefficients.size(), other.coefficients.size()); i++) {
            double coefficient1 = i < coefficients.size() ? coefficients[i] : 0
            double coefficient2 = i < other.coefficients.size() ? other.coefficients[i] : 0
            newCoefficients.add(coefficient1 + coefficient2)
        }
        return new Polynomial(newCoefficients)
    }

    // Method to subtract two polynomials.
    Polynomial subtract(Polynomial other) {
        List<Double> newCoefficients = []
        for (int i = 0; i < Math.max(coefficients.size(), other.coefficients.size()); i++) {
            double coefficient1 = i < coefficients.size() ? coefficients[i] : 0
            double coefficient2 = i < other.coefficients.size() ? other.coefficients[i] : 0
            newCoefficients.add(coefficient1 - coefficient2)
        }