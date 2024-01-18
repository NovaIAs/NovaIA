```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>

using namespace std;

// A lambda function to calculate the factorial of a number.
auto factorial = [](int n) {
    return (n == 0) ? 1 : n * factorial(n - 1);
};

// A class representing a polynomial with real coefficients.
class Polynomial {
public:
    // The constructor takes a vector of coefficients as input.
    explicit Polynomial(const vector<double>& coefficients)
        : coefficients_(coefficients)
    { }

    // Evaluate the polynomial at a given value of x.
    double operator()(double x) const {
        double result = 0;
        for (size_t i = 0; i < coefficients_.size(); ++i) {
            result += coefficients_[i] * pow(x, i);
        }
        return result;
    }

    // Calculate the derivative of the polynomial.
    Polynomial derivative() const {
        vector<double> derivatives(coefficients_.size() - 1);
        for (size_t i = 1; i < coefficients_.size(); ++i) {
            derivatives[i - 1] = coefficients_[i] * i;
        }
        return Polynomial(derivatives);
    }

    // Calculate the integral of the polynomial.
    Polynomial integral() const {
        vector<double> integrals(coefficients_.size() + 1);
        integrals[0] = 0;
        for (size_t i = 1; i < coefficients_.size() + 1; ++i) {
            integrals[i] = coefficients_[i - 1] / i;
        }
        return Polynomial(integrals);
    }

private:
    vector<double> coefficients_;
};

int main() {
    // Create a polynomial with coefficients [1, 2, 3].
    Polynomial p({1, 2, 3});

    // Evaluate the polynomial at x = 2.
    double result = p(2);

    // Calculate the derivative of the polynomial.
    Polynomial derivative = p.derivative();

    // Calculate the integral of the polynomial.
    Polynomial integral = p.integral();

    // Print the polynomial, its derivative, and its integral.
    cout << "Polynomial: ";
    for (size_t i = 0; i < p.coefficients_.size(); ++i) {
        cout << p.coefficients_[i] << "x^" << i << " + ";
    }
    cout << endl;

    cout << "Derivative: ";
    for (size_t i = 0; i < derivative.coefficients_.size(); ++i) {
        cout << derivative.coefficients_[i] << "x^" << i << " + ";
    }
    cout << endl;

    cout << "Integral: ";
    for (size_t i = 0; i < integral.coefficients_.size(); ++i) {
        cout << integral.coefficients_[i] << "x^" << i << " + ";
    }
    cout << endl;

    return 0;
}
```

This code defines a class `Polynomial` that represents a polynomial with real coefficients. It has methods to evaluate the polynomial at a given value of x, to calculate the derivative of the polynomial, and to calculate the integral of the polynomial.

The main function creates a polynomial with coefficients [1, 2, 3], evaluates it at x = 2, calculates its derivative and integral, and then prints the polynomial, its derivative, and its integral.