```c++
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <sstream>

using namespace std;

struct Fraction {
  long long numerator, denominator;

  Fraction() : numerator(0), denominator(1) {}
  Fraction(long long numerator, long long denominator)
      : numerator(numerator), denominator(denominator) {}

  Fraction operator+(const Fraction& other) const {
    long long newNumerator = numerator * other.denominator + denominator * other.numerator;
    long long newDenominator = denominator * other.denominator;
    Fraction result(newNumerator, newDenominator);
    result.simplify();
    return result;
  }

  Fraction operator-(const Fraction& other) const {
    long long newNumerator = numerator * other.denominator - denominator * other.numerator;
    long long newDenominator = denominator * other.denominator;
    Fraction result(newNumerator, newDenominator);
    result.simplify();
    return result;
  }

  Fraction operator*(const Fraction& other) const {
    long long newNumerator = numerator * other.numerator;
    long long newDenominator = denominator * other.denominator;
    Fraction result(newNumerator, newDenominator);
    result.simplify();
    return result;
  }

  Fraction operator/(const Fraction& other) const {
    long long newNumerator = numerator * other.denominator;
    long long newDenominator = denominator * other.numerator;
    Fraction result(newNumerator, newDenominator);
    result.simplify();
    return result;
  }

  void simplify() {
    long long gcd = __gcd(numerator, denominator);
    numerator /= gcd;
    denominator /= gcd;
    if (denominator < 0) {
      numerator *= -1;
      denominator *= -1;
    }
  }

  string toString() const {
    stringstream ss;
    ss << numerator << "/" << denominator;
    return ss.str();
  }
};

int main() {
  Fraction a(1, 2);
  Fraction b(3, 4);

  cout << "a = " << a.toString() << endl;
  cout << "b = " << b.toString() << endl;

  Fraction c = a + b;
  cout << "a + b = " << c.toString() << endl;

  Fraction d = a - b;
  cout << "a - b = " << d.toString() << endl;

  Fraction e = a * b;
  cout << "a * b = " << e.toString() << endl;

  Fraction f = a / b;
  cout << "a / b = " << f.toString() << endl;

  return 0;
}
```

This code defines a Fraction struct that represents a rational number (a number that can be expressed as a quotient of two integers). The struct has two member variables: numerator and denominator, which represent the numerator and denominator of the fraction, respectively.

The struct also has several member functions, which allow you to perform basic arithmetic operations on fractions. These functions are implemented using the standard arithmetic operators (+, -, *, and /).

The Fraction struct also has a simplify() function, which simplifies the fraction by dividing both the numerator and denominator by their greatest common divisor.

The main() function creates two Fraction objects, a and b, and then uses the +,-,*, and / operators to perform basic arithmetic operations on them. The results of these operations are printed to the console.

This code is a good example of how to create a custom data type in C++ and how to use it to perform basic arithmetic operations. It is also a good example of how to use the standard arithmetic operators to overload custom functions.