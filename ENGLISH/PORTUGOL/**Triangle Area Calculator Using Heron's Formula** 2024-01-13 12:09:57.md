```portugal
// This program calculates the area of a triangle using Heron's formula.

// Declare the variables.
real a, b, c, s, area;

// Get the input from the user.
print("Enter the lengths of the three sides of the triangle: ");
read(a, b, c);

// Calculate the semi-perimeter of the triangle.
s = (a + b + c) / 2;

// Calculate the area of the triangle using Heron's formula.
area = sqrt(s * (s - a) * (s - b) * (s - c));

// Print the area of the triangle.
print("The area of the triangle is: ", area);

```

**Explanation:**

This program calculates the area of a triangle using Heron's formula. The program first declares the variables that will be used in the calculation. Then, it gets the input from the user, which is the lengths of the three sides of the triangle. Next, it calculates the semi-perimeter of the triangle, which is the sum of the three sides divided by 2. Then, it calculates the area of the triangle using Heron's formula, which is the square root of the semi-perimeter multiplied by the semi-perimeter minus each side of the triangle. Finally, the program prints the area of the triangle to the user.