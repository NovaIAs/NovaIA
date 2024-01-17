```lua
-- Create a custom type for representing complex numbers.
Complex = {}
function Complex:new(real, imag)
  return setmetatable({}, Complex)
end
function Complex:__add__(other)
  return Complex:new(self.real + other.real, self.imag + other.imag)
end
function Complex:__sub__(other)
  return Complex:new(self.real - other.real, self.imag - other.imag)
end
function Complex:__mul__(other)
  if type(other) == "number" then
    return Complex:new(self.real * other, self.imag * other)
  elseif type(other) == "Complex" then
    return Complex:new(
      self.real * other.real - self.imag * other.imag,
      self.real * other.imag + self.imag * other.real
    )
  else
    error("Invalid operand type for multiplication.")
  end
end
function Complex:__div__(other)
  if type(other) == "number" then
    return Complex:new(self.real / other, self.imag / other)
  elseif type(other) == "Complex" then
    local d = other.real^2 + other.imag^2
    return Complex:new(
      (self.real * other.real + self.imag * other.imag) / d,
      (self.imag * other.real - self.real * other.imag) / d
    )
  else
    error("Invalid operand type for division.")
  end
end
function Complex:__tostring__()
  return string.format("%f + %fi", self.real, self.imag)
end

-- Create a function to calculate the roots of a quadratic equation.
function quadratic_roots(a, b, c)
  local discriminant = b^2 - 4 * a * c
  if discriminant < 0 then
    return "No real roots."
  elseif discriminant == 0 then
    return -b / (2 * a)
  else
    local sqrt_d = math.sqrt(discriminant)
    return {
      (-b + sqrt_d) / (2 * a),
      (-b - sqrt_d) / (2 * a)
    }
  end
end

-- Create a function to check if a number is prime.
function is_prime(n)
  if n < 2 then
    return false
  end
  for i = 2, math.floor(math.sqrt(n)) do
    if n % i == 0 then
      return false
    end
  end
  return true
end

-- Create a table to store the first 100 prime numbers.
primes = {}
for i = 2, 100 do
  if is_prime(i) then
    table.insert(primes, i)
  end
end

-- Create a function to print a multiplication table for a given number.
function print_multiplication_table(n)
  for i = 1, 10 do
    print(string.format("%d x %d = %d", n, i, n * i))
  end
end

-- Test the code.
local z1 = Complex:new(3, 4)
local z2 = Complex:new(5, -2)
print("z1 + z2 =", z1 + z2)
print("z1 - z2 =", z1 - z2)
print("z1 * z2 =", z1 * z2)
print("z1 / z2 =", z1 / z2)
print("Quadratic roots of 1, -5, 6:", quadratic_roots(1, -5, 6))
print("Is 7 prime?", is_prime(7))
print("Is 10 prime?", is_prime(10))
print("First 100 prime numbers:")
for i, prime in ipairs(primes) do
  print(prime)
end
print("Multiplication table for 7:")
print_multiplication_table(7)
```

Explanation:

* The code defines a custom type called `Complex` for representing complex numbers.
* It defines arithmetic operators (`+, -, *, /`) for complex numbers.
* It defines a function `quadratic_roots` to calculate the roots of a quadratic equation.
* It defines a function `is_prime` to check if a number is prime.
* It defines a table `primes` to store the first 100 prime numbers.
* It defines a function `print_multiplication_table` to print a multiplication table for a given number.
* The code tests the functions by performing various operations on complex numbers, calculating quadratic roots, checking for primality, printing prime numbers, and printing a multiplication table.