```cool
class Complex

  -- Attributes
  private Integer real;
  private Integer imaginary;

  -- Constructor
  new (Integer real, Integer imaginary) is
    self.real <- real;
    self.imaginary <- imaginary;
  end new;

  -- Methods
  public Integer get_real() is
    return self.real;
  end get_real;

  public Integer get_imaginary() is
    return self.imaginary;
  end get_imaginary;

  public Complex add(Complex other) is
    Integer new_real := self.real + other.get_real();
    Integer new_imaginary := self.imaginary + other.get_imaginary();
    return new Complex.new(new_real, new_imaginary);
  end add;

  public Complex subtract(Complex other) is
    Integer new_real := self.real - other.get_real();
    Integer new_imaginary := self.imaginary - other.get_imaginary();
    return new Complex.new(new_real, new_imaginary);
  end subtract;

  public Complex multiply(Complex other) is
    Integer new_real := (self.real * other.get_real()) - (self.imaginary * other.get_imaginary());
    Integer new_imaginary := (self.real * other.get_imaginary()) + (self.imaginary * other.get_real());
    return new Complex.new(new_real, new_imaginary);
  end multiply;

  public Complex divide(Complex other) is
    Integer denominator := (other.get_real()^2) + (other.get_imaginary()^2);
    Integer new_real := ((self.real * other.get_real()) + (self.imaginary * other.get_imaginary())) / denominator;
    Integer new_imaginary := ((self.imaginary * other.get_real()) - (self.real * other.get_imaginary())) / denominator;
    return new Complex.new(new_real, new_imaginary);
  end divide;

  public String to_string() is
    String real_str := String.from_int(self.real);
    String imaginary_str := String.from_int(self.imaginary);
    if self.imaginary >= 0 then
      return real_str + " + " + imaginary_str + "i";
    else
      return real_str + " - " + (-self.imaginary) + "i";
    end if;
  end to_string;
end Complex;
```

This code defines a class called `Complex` in COOL, which represents complex numbers. Complex numbers have two parts: a real part and an imaginary part. The `Complex` class has two private attributes, `real` and `imaginary`, which store the real and imaginary parts of the complex number, respectively.

The class also has a constructor, which takes two integers as arguments and initializes the `real` and `imaginary` attributes with those values.

The class has several public methods, which allow you to perform various operations on complex numbers. These methods include:

* `get_real()`: Returns the real part of the complex number.
* `get_imaginary()`: Returns the imaginary part of the complex number.
* `add(Complex other)`: Adds two complex numbers together and returns the result.
* `subtract(Complex other)`: Subtracts one complex number from another and returns the result.
* `multiply(Complex other)`: Multiplies two complex numbers together and returns the result.
* `divide(Complex other)`: Divides one complex number by another and returns the result.
* `to_string()`: Converts the complex number to a string representation.

To use the `Complex` class, you can create a new instance of the class using the `new` constructor. For example, the following code creates a new complex number with a real part of 3 and an imaginary part of 4:

```cool
Complex c := new Complex.new(3, 4);
```

You can then use the public methods of the `Complex` class to perform various operations on the complex number. For example, the following code prints the real and imaginary parts of the complex number `c`:

```cool
System.print_int(c.get_real());
System.print_string(" ");
System.print_int(c.get_imaginary());
System.print_ln();
```

The output of the above code would be:

```
3 4
```

The `Complex` class can be used to perform a variety of mathematical operations on complex numbers. It is a powerful tool for working with complex numbers in COOL.