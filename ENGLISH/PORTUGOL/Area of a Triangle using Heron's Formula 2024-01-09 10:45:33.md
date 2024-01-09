```portugal
// This program calculates the area of a triangle using Heron's formula.

#include <stdio.h>
#include <math.h>

int main() {
    // Declare the variables.
    double a, b, c, s, area;

    // Prompt the user to enter the lengths of the sides of the triangle.
    printf("Enter the lengths of the sides of the triangle:\n");
    scanf("%lf %lf %lf", &a, &b, &c);

    // Calculate the semi-perimeter of the triangle.
    s = (a + b + c) / 2;

    // Calculate the area of the triangle using Heron's formula.
    area = sqrt(s * (s - a) * (s - b) * (s - c));

    // Print the area of the triangle.
    printf("The area of the triangle is %lf square units.\n", area);

    return 0;
}
```

Explanation:

* The `#include` statements include the standard input/output library and the math library.
* The `main()` function is the entry point of the program.
* The `printf()` function is used to prompt the user to enter the lengths of the sides of the triangle.
* The `scanf()` function is used to read the lengths of the sides of the triangle from the user.
* The `s` variable is used to store the semi-perimeter of the triangle.
* The `area` variable is used to store the area of the triangle.
* The `sqrt()` function is used to calculate the square root of a number.
* The `printf()` function is used to print the area of the triangle to the console.