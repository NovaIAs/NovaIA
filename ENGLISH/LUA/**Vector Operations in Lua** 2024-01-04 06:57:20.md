```lua
-- This Lua code defines a class called "Vector" that represents a two-dimensional vector with various operations and properties.

local Vector = {}

-- Constructor function to create a new Vector object.
function Vector:new(x, y)
    local self = {}
    self.x = x or 0
    self.y = y or 0
    setmetatable(self, self)
    return self
end

-- Method to add two vectors together.
function Vector:add(other)
    return Vector:new(self.x + other.x, self.y + other.y)
end

-- Method to subtract one vector from another.
function Vector:subtract(other)
    return Vector:new(self.x - other.x, self.y - other.y)
end

-- Method to multiply a vector by a scalar value.
function Vector:multiply(scalar)
    return Vector:new(self.x * scalar, self.y * scalar)
end

-- Method to divide a vector by a scalar value.
function Vector:divide(scalar)
    return Vector:new(self.x / scalar, self.y / scalar)
end

-- Method to calculate the dot product of two vectors.
function Vector:dot(other)
    return self.x * other.x + self.y * other.y
end

-- Method to calculate the cross product of two vectors.
function Vector:cross(other)
    return Vector:new(self.x * other.y - self.y * other.x)
end

-- Method to calculate the magnitude of the vector.
function Vector:magnitude()
    return math.sqrt(self.x^2 + self.y^2)
end

-- Method to normalize the vector to have a magnitude of 1.
function Vector:normalize()
    local magnitude = self:magnitude()
    if magnitude == 0 then
        return self
    end
    return Vector:new(self.x / magnitude, self.y / magnitude)
end

-- Method to calculate the angle between two vectors in radians.
function Vector:angle(other)
    local dotProduct = self:dot(other)
    local magnitudes = self:magnitude() * other:magnitude()
    return math.acos(dotProduct / magnitudes)
end

-- Method to convert the vector to a string representation.
function Vector:to_string()
    return string.format("(%.2f, %.2f)", self.x, self.y)
end

-- Create two Vector objects.
local vector1 = Vector:new(3, 4)
local vector2 = Vector:new(5, 6)

-- Perform various operations using the methods defined in the Vector class.
local vectorSum = vector1:add(vector2)
local vectorDifference = vector1:subtract(vector2)
local vectorProduct = vector1:multiply(2)
local vectorQuotient = vector2:divide(2)
local dotProduct = vector1:dot(vector2)
local crossProduct = vector1:cross(vector2)
local vectorMagnitude = vector1:magnitude()
local normalizedVector1 = vector1:normalize()
local angleBetweenVectors = vector1:angle(vector2)

-- Display the results of the operations.
print("Vector Sum: " .. vectorSum:to_string())
print("Vector Difference: " .. vectorDifference:to_string())
print("Vector Product: " .. vectorProduct:to_string())
print("Vector Quotient: " .. vectorQuotient:to_string())
print("Dot Product: " .. dotProduct)
print("Cross Product: " .. crossProduct:to_string())
print("Vector Magnitude: " .. vectorMagnitude)
print("Normalized Vector 1: " .. normalizedVector1:to_string())
print("Angle Between Vectors: " .. angleBetweenVectors)
```

Explanation:

1. We define a class called "Vector" that represents a two-dimensional vector.

2. The constructor function "new" allows us to create new Vector objects by providing the x and y components as arguments.

3. We define various methods for the Vector class, including addition, subtraction, multiplication, division, dot product, cross product, magnitude calculation, normalization, and angle calculation.

4. We create two Vector objects, vector1 and vector2, and use the methods to perform various operations on them.

5. We display the results of the operations using the "to_string" method, which converts the Vector objects to a string representation.

This code demonstrates the use of object-oriented programming in Lua to define a class representing a two-dimensional vector and perform various operations on vectors.