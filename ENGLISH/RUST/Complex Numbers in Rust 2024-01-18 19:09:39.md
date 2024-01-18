```rust
// Create a custom type to represent a complex number.
#[derive(Debug, PartialEq)]
struct Complex {
    real: f64,
    imag: f64,
}

// Implement addition for the `Complex` type.
impl std::ops::Add for Complex {
    type Output = Complex;

    fn add(self, other: Complex) -> Complex {
        Complex {
            real: self.real + other.real,
            imag: self.imag + other.imag,
        }
    }
}

// Implement subtraction for the `Complex` type.
impl std::ops::Sub for Complex {
    type Output = Complex;

    fn sub(self, other: Complex) -> Complex {
        Complex {
            real: self.real - other.real,
            imag: self.imag - other.imag,
        }
    }
}

// Implement multiplication for the `Complex` type.
impl std::ops::Mul for Complex {
    type Output = Complex;

    fn mul(self, other: Complex) -> Complex {
        Complex {
            real: self.real * other.real - self.imag * other.imag,
            imag: self.real * other.imag + self.imag * other.real,
        }
    }
}

// Implement division for the `Complex` type.
impl std::ops::Div for Complex {
    type Output = Complex;

    fn div(self, other: Complex) -> Complex {
        let denominator = other.real * other.real + other.imag * other.imag;

        Complex {
            real: (self.real * other.real + self.imag * other.imag) / denominator,
            imag: (self.imag * other.real - self.real * other.imag) / denominator,
        }
    }
}

// Define a function to calculate the absolute value of a complex number.
fn abs(c: Complex) -> f64 {
    (c.real * c.real + c.imag * c.imag).sqrt()
}

// Define a function to calculate the argument of a complex number.
fn arg(c: Complex) -> f64 {
    c.imag.atan2(c.real)
}

// Define a function to convert a complex number to a string.
fn to_string(c: Complex) -> String {
    format!("{} + {}i", c.real, c.imag)
}

// Create a few complex numbers.
let c1 = Complex { real: 1.0, imag: 2.0 };
let c2 = Complex { real: 3.0, imag: 4.0 };

// Perform some operations on the complex numbers.
let c3 = c1 + c2;
let c4 = c1 - c2;
let c5 = c1 * c2;
let c6 = c1 / c2;

// Print the results.
println!("c1 = {}", to_string(c1));
println!("c2 = {}", to_string(c2));
println!("c3 = {}", to_string(c3));
println!("c4 = {}", to_string(c4));
println!("c5 = {}", to_string(c5));
println!("c6 = {}", to_string(c6));

// Calculate the absolute value and argument of c1.
let abs_c1 = abs(c1);
let arg_c1 = arg(c1);

// Print the results.
println!("Absolute value of c1 = {}", abs_c1);
println!("Argument of c1 = {}", arg_c1);
```

This code defines a custom `Complex` type to represent complex numbers, with methods for addition, subtraction, multiplication, and division. It also defines functions to calculate the absolute value and argument of a complex number, and to convert a complex number to a string. The code then creates a few complex numbers, performs some operations on them, and prints the results.