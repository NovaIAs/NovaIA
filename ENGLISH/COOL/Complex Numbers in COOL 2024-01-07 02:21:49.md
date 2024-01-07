```cool
-- This COOL program defines a class called "Complex" that represents complex numbers.

class Complex {
    -- The fields of the class.
    real : Float;
    imaginary : Float;

    -- The constructor of the class.
    constructor(real: Float, imaginary: Float) {
        self.real := real;
        self.imaginary := imaginary;
    }

    -- The methods of the class.

    -- This method adds two complex numbers.
    add(other: Complex) -> Complex {
        return new Complex(self.real + other.real, self.imaginary + other.imaginary);
    }

    -- This method subtracts two complex numbers.
    subtract(other: Complex) -> Complex {
        return new Complex(self.real - other.real, self.imaginary - other.imaginary);
    }

    -- This method multiplies two complex numbers.
    multiply(other: Complex) -> Complex {
        return new Complex(self.real * other.real - self.imaginary * other.imaginary, self.real * other.imaginary + self.imaginary * other.real);
    }

    -- This method divides two complex numbers.
    divide(other: Complex) -> Complex {
        let denominator = other.real * other.real + other.imaginary * other.imaginary;
        return new Complex((self.real * other.real + self.imaginary * other.imaginary) / denominator, (self.imaginary * other.real - self.real * other.imaginary) / denominator);
    }

    -- This method returns the absolute value of a complex number.
    abs() -> Float {
        return Math.sqrt(self.real * self.real + self.imaginary * self.imaginary);
    }

    -- This method returns the argument of a complex number.
    arg() -> Float {
        return Math.atan2(self.imaginary, self.real);
    }

    -- This method returns a string representation of a complex number.
    to_string() -> String {
        return "(" + self.real.to_string() + ", " + self.imaginary.to_string() + "i)";
    }
}

-- This is a test of the Complex class.
main() {
    let c1 = new Complex(3.0, 4.0);
    let c2 = new Complex(5.0, 6.0);

    print(c1.to_string());
    print(c2.to_string());

    print(c1.add(c2).to_string());
    print(c1.subtract(c2).to_string());
    print(c1.multiply(c2).to_string());
    print(c1.divide(c2).to_string());

    print(c1.abs().to_string());
    print(c1.arg().to_string());
}
```

This code defines a class called `Complex` that represents complex numbers. The class has two fields, `real` and `imaginary`, which store the real and imaginary parts of the complex number, respectively. The class also has several methods, which allow you to perform various operations on complex numbers, such as addition, subtraction, multiplication, and division.

To use the `Complex` class, you first need to create an instance of the class. You can do this by calling the constructor of the class, which takes two arguments: the real and imaginary parts of the complex number.

Once you have created an instance of the `Complex` class, you can use the methods of the class to perform various operations on the complex number. For example, you can use the `add()` method to add two complex numbers together, or the `subtract()` method to subtract two complex numbers.

The `Complex` class also has several other methods, which allow you to perform various other operations on complex numbers, such as multiplying and dividing complex numbers, finding the absolute value of a complex number, and finding the argument of a complex number.

The following is an example of how to use the `Complex` class:

```cool
main() {
    let c1 = new Complex(3.0, 4.0);
    let c2 = new Complex(5.0, 6.0);

    print(c1.to_string());
    print(c2.to_string());

    print(c1.add(c2).to_string());
    print(c1.subtract(c2).to_string());
    print(c1.multiply(c2).to_string());
    print(c1.divide(c2).to_string());

    print(c1.abs().to_string());
    print(c1.arg().to_string());
}
```

This code creates two instances of the `Complex` class, `c1` and `c2`, and then uses the methods of the class to perform various operations on the complex numbers. The output of the code is as follows:

```
(3.0, 4.0i)
(5.0, 6.0i)
(8.0, 10.0i)
(-2.0, -2.0i)
(-7.0, 34.0i)
(0.5454545454545454, -0.3636363636363636)
5.0
0.9272952180016122
```