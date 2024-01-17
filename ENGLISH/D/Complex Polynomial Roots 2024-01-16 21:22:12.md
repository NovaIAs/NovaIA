```d
import std.stdio, std.string, std.algorithm, std.math;

// Define a struct to represent a complex number
struct Complex {
    real: real;
    imag: real;

    // Constructor
    this(real r, real i) pure nothrow {
        real = r;
        imag = i;
    }

    // Default constructor
    this() pure nothrow {
        this(0, 0);
    }

    // Addition operator
    Complex opAdd(Complex other) pure nothrow {
        return new Complex(real + other.real, imag + other.imag);
    }

    // Subtraction operator
    Complex opSub(Complex other) pure nothrow {
        return new Complex(real - other.real, imag - other.imag);
    }

    // Multiplication operator
    Complex opMul(Complex other) pure nothrow {
        return new Complex(real * other.real - imag * other.imag,
                           real * other.imag + imag * other.real);
    }

    // Division operator
    Complex opDiv(Complex other) pure nothrow {
        real denominator = other.real * other.real + other.imag * other.imag;
        return new Complex((real * other.real + imag * other.imag) / denominator,
                           (imag * other.real - real * other.imag) / denominator);
    }

    // Conjugate operator
    Complex opConjugate() pure nothrow {
        return new Complex(real, -imag);
    }

    // Absolute value (magnitude)
    real abs() pure nothrow {
        return sqrt(real * real + imag * imag);
    }

    // Phase angle (argument)
    real arg() pure nothrow {
        return atan2(imag, real);
    }

    // String representation
    string to!string() pure nothrow {
        return "(" ~ real ~ (imag >= 0 ? "+" : "-") ~ abs(imag) ~ "i" ~ ")";
    }
}

// Define a function to calculate the complex roots of a quadratic equation
Complex[] quadraticRoots(real a, real b, real c) pure nothrow {
    Complex[] roots = new Complex[2];
    real discriminant = b * b - 4 * a * c;

    if (discriminant < 0) {
        // Complex roots
        real sqrtDiscriminant = sqrt(-discriminant);
        roots[0] = new Complex(-b / (2 * a), sqrtDiscriminant / (2 * a));
        roots[1] = new Complex(-b / (2 * a), -sqrtDiscriminant / (2 * a));
    } else {
        // Real roots
        real sqrtDiscriminant = sqrt(discriminant);
        roots[0] = new Complex((-b + sqrtDiscriminant) / (2 * a), 0);
        roots[1] = new Complex((-b - sqrtDiscriminant) / (2 * a), 0);
    }

    return roots;
}

// Define a function to calculate the roots of a complex polynomial
Complex[] polynomialRoots(Complex[] coefficients) pure nothrow {
    int degree = coefficients.length - 1;
    Complex[] roots = new Complex[degree];
    Complex[] quotientCoefficients = new Complex[degree];

    // Find the first root using quadratic formula
    Complex[] quadraticRoots = quadraticRoots(coefficients[degree], coefficients[degree - 1], coefficients[degree - 2]);

    // Store the first root and update the polynomial coefficients
    roots[0] = quadraticRoots[0];
    for (int i = 0; i < degree; i++) {
        quotientCoefficients[i] = coefficients[i + 2];
    }

    // Find the remaining roots using synthetic division
    for (int i = 1; i < degree; i++) {
        Complex root = quadraticRoots[1];
        roots[i] = root;

        // Update the polynomial coefficients
        for (int j = degree - 1; j >= 0; j--) {
            quotientCoefficients[j] = quotientCoefficients[j] + root * coefficients[j + 1];
        }
    }

    return roots;
}

void main() {
    // Define a complex polynomial
    Complex[] coefficients = {
        new Complex(1, 0),
        new Complex(-2, 0),
        new Complex(1, 0)
    };

    // Find the roots of the polynomial
    Complex[] roots = polynomialRoots(coefficients);

    // Print the roots
    foreach (root; roots) {
        writeln(root);
    }
}
```

This code implements a function to calculate the roots of a complex polynomial using synthetic division. It first finds the first root using the quadratic formula and then uses synthetic division to find the remaining roots.

The code is explained as follows:

1. Define a `Complex` struct to represent complex numbers.
2. Define addition, subtraction, multiplication, division, and conjugate operators for the `Complex` struct.
3. Define an `abs()` function to calculate the absolute value of a complex number.
4. Define an `arg()` function to calculate the phase angle of a complex number.
5. Define a `to!string()` function to convert a complex number to a string.
6. Define a `quadraticRoots()` function to calculate the complex roots of a quadratic equation.
7. Define a `polynomialRoots()` function to calculate the roots of a complex polynomial using synthetic division.
8. In the `main()` function, define a complex polynomial and find its roots using the `polynomialRoots()` function.
9. Print the roots of the polynomial.