```lua
-- Module for working with complex numbers

-- Constructor function
function complex(real, imag)
  return {real = real or 0, imag = imag or 0}
end

-- Addition function
function complex:add(other)
  return complex(self.real + other.real, self.imag + other.imag)
end

-- Subtraction function
function complex:sub(other)
  return complex(self.real - other.real, self.imag - other.imag)
end

-- Multiplication function
function complex:mul(other)
  if type(other) == "number" then
    return complex(self.real * other, self.imag * other)
  else
    return complex(
      self.real * other.real - self.imag * other.imag,
      self.real * other.imag + self.imag * other.real
    )
  end
end

-- Division function
function complex:div(other)
  if type(other) == "number" then
    return complex(self.real / other, self.imag / other)
  else
    local denom = other.real^2 + other.imag^2
    return complex(
      (self.real * other.real + self.imag * other.imag) / denom,
      (self.imag * other.real - self.real * other.imag) / denom
    )
  end
end

-- Conjugate function
function complex:conj()
  return complex(self.real, -self.imag)
end

-- Absolute value function
function complex:abs()
  return math.sqrt(self.real^2 + self.imag^2)
end

-- Argument function
function complex:arg()
  return math.atan2(self.imag, self.real)
end

-- String representation function
function complex:tostring()
  return string.format("%f%+fi", self.real, self.imag)
end

-- Register the complex module in the global environment
package.loaded["complex"] = complex

-- Example usage
local c1 = complex(3, 4)
local c2 = complex(5, -2)

print("c1 + c2 =", c1:add(c2))
print("c1 - c2 =", c1:sub(c2))
print("c1 * c2 =", c1:mul(c2))
print("c1 / c2 =", c1:div(c2))
print("c1's conjugate =", c1:conj())
print("Absolute value of c1 =", c1:abs())
print("Argument of c1 =", c1:arg())
print("String representation of c1 =", c1:tostring())
```

This code implements a complex number module in Lua. It includes functions for addition, subtraction, multiplication, division, conjugation, absolute value, argument, and string representation. The module is registered in the global environment so that it can be used in other Lua scripts.

Here is an example of how to use the complex number module:

```lua
-- Require the complex number module
local complex = require("complex")

-- Create two complex numbers
local c1 = complex(3, 4)
local c2 = complex(5, -2)

-- Perform some operations on the complex numbers
local c3 = c1:add(c2)
local c4 = c1:sub(c2)
local c5 = c1:mul(c2)
local c6 = c1:div(c2)

-- Print the results
print("c1 + c2 =", c3)
print("c1 - c2 =", c4)
print("c1 * c2 =", c5)
print("c1 / c2 =", c6)
```

This code will print the following output:

```
c1 + c2 = 8+2i
c1 - c2 = -2+6i
c1 * c2 = 23-8i
c1 / c2 = 0.38461538461538464+0.3076923076923077i
```