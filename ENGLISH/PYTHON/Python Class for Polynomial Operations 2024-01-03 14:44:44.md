```python
class Polynomial:
    """
    Class representing a polynomial.

    Attributes:
        coefficients: A list of the coefficients of the polynomial, in order of
            decreasing degree. The coefficient of the highest degree term is the
            first element of the list.
    """

    def __init__(self, coefficients):
        """
        Constructs a new polynomial from a list of coefficients.

        Args:
            coefficients: A list of the coefficients of the polynomial, in order of
                decreasing degree. The coefficient of the highest degree term is the
                first element of the list.
        """

        self.coefficients = coefficients

    def evaluate(self, x):
        """
        Evaluates the polynomial at a given value of x.

        Args:
            x: The value of x at which to evaluate the polynomial.

        Returns:
            The value of the polynomial at x.
        """

        result = 0
        for coefficient in self.coefficients:
            result = result * x + coefficient
        return result

    def derivative(self):
        """
        Returns the derivative of the polynomial.

        Returns:
            A new polynomial that is the derivative of this polynomial.
        """

        derivative_coefficients = []
        for i in range(1, len(self.coefficients)):
            derivative_coefficients.append(i * self.coefficients[i])
        return Polynomial(derivative_coefficients)

    def __add__(self, other):
        """
        Adds two polynomials together.

        Args:
            other: The other polynomial to add to this one.

        Returns:
            A new polynomial that is the sum of this polynomial and the other
            polynomial.
        """

        new_coefficients = []
        for i in range(max(len(self.coefficients), len(other.coefficients))):
            self_coefficient = self.coefficients[i] if i < len(self.coefficients) else 0
            other_coefficient = other.coefficients[i] if i < len(other.coefficients) else 0
            new_coefficients.append(self_coefficient + other_coefficient)
        return Polynomial(new_coefficients)

    def __sub__(self, other):
        """
        Subtracts one polynomial from another.

        Args:
            other: The polynomial to subtract from this one.

        Returns:
            A new polynomial that is the difference of this polynomial and the
            other polynomial.
        """

        new_coefficients = []
        for i in range(max(len(self.coefficients), len(other.coefficients))):
            self_coefficient = self.coefficients[i] if i < len(self.coefficients) else 0
            other_coefficient = other.coefficients[i] if i < len(other.coefficients) else 0
            new_coefficients.append(self_coefficient - other_coefficient)
        return Polynomial(new_coefficients)

    def __mul__(self, other):
        """
        Multiplies two polynomials together.

        Args:
            other: The other polynomial to multiply this one by.

        Returns:
            A new polynomial that is the product of this polynomial and the other
            polynomial.
        """

        new_coefficients = [0] * (len(self.coefficients) + len(other.coefficients) - 1)
        for i in range(len(self.coefficients)):
            for j in range(len(other.coefficients)):
                new_coefficients[i + j] += self.coefficients[i] * other.coefficients[j]
        return Polynomial(new_coefficients)

    def __str__(self):
        """
        Returns a string representation of the polynomial.

        Returns:
            A string representation of the polynomial.
        """

        polynomial_string = ""
        for i in range(len(self.coefficients) - 1, -1, -1):
            coefficient = self.coefficients[i]
            if coefficient != 0:
                polynomial_string += str(coefficient)
                if i > 0:
                    polynomial_string += "x^" + str(i)
                if i > 1:
                    polynomial_string += " "
        return polynomial_string

def main():
    """
    Main function.
    """

    # Create two polynomials.
    p = Polynomial([1, 2, 3])
    q = Polynomial([4, 5, 6])

    # Evaluate the polynomials at x = 2.
    p_value = p.evaluate(2)
    q_value = q.evaluate(2)

    # Print the polynomials and their values.
    print("p(x) =", p)
    print("p(2) =", p_value)
    print("q(x) =", q)
    print("q(2) =", q_value)

    # Add the polynomials together.
    r = p + q

    # Print the sum of the polynomials.
    print("p(x) + q(x) =", r)

    # Subtract the polynomials from each other.
    s = p - q

    # Print the difference of the polynomials.
    print("p(x) - q(x) =", s)

    # Multiply the polynomials together.
    t = p * q

    # Print the product of the polynomials.
    print("p(x) * q(x) =", t)

if __name__ == "__main__":
    main()
```

This code is a Python implementation of a polynomial class. A polynomial is a mathematical expression that is a sum of terms, each of which is a constant multiplied by a variable raised to a non-negative integer power.

The Polynomial class has the following attributes:

* `coefficients`: A list of the coefficients of the polynomial, in order of decreasing degree. The coefficient of the highest degree term is the first element of the list.

The Polynomial class has the following methods:

* `__init__()`: Constructs a new polynomial from a list of coefficients.
* `evaluate()`: Evaluates the polynomial at a given value of x.
* `derivative()`: Returns the derivative of the polynomial.
* `__add__()`: Adds two polynomials together.
* `__sub__()`: Subtracts one polynomial from another.
* `__mul__()`: Multiplies two polynomials together.
* `__str__()`: Returns a string representation of the polynomial.

The `main()` function creates two polynomials, evaluates them at x = 2, adds them together, subtracts them from each other, and multiplies them together. The results are then printed to the console.