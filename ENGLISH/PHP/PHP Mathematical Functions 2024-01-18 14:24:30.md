```php

<?php

// Calculate the factorial of a number

// Define the function to calculate the factorial

function factorial($num) {
  if ($num == 0) {
    return 1;
  } else {
    return $num * factorial($num-1);
  }
}

// Get the input from the user

$input = readline('Enter a number: ');

// Calculate the factorial of the input

$factorial = factorial($input);

// Print the result

echo "The factorial of $input is $factorial";

// Calculate the area of a triangle

// Define the function to calculate the area of a triangle

function areaOfTriangle($base, $height) {
  return 0.5 * $base * $height;
}

// Get the input from the user

$base = readline('Enter the base of the triangle: ');
$height = readline('Enter the height of the triangle: ');

// Calculate the area of the triangle

$area = areaOfTriangle($base, $height);

// Print the result

echo "The area of the triangle is $area";

// Calculate the volume of a sphere

// Define the function to calculate the volume of a sphere

function volumeOfSphere($radius) {
  return (4/3) * pi() * ($radius**3);
}

// Get the input from the user

$radius = readline('Enter the radius of the sphere: ');

// Calculate the volume of the sphere

$volume = volumeOfSphere($radius);

// Print the result

echo "The volume of the sphere is $volume";

// Convert a temperature from Celsius to Fahrenheit

// Define the function to convert a temperature from Celsius to Fahrenheit

function celsiusToFahrenheit($celsius) {
  return ($celsius * 9/5) + 32;
}

// Get the input from the user

$celsius = readline('Enter the temperature in Celsius: ');

// Convert the temperature to Fahrenheit

$fahrenheit = celsiusToFahrenheit($celsius);

// Print the result

echo "The temperature in Fahrenheit is $fahrenheit";

// Convert a temperature from Fahrenheit to Celsius

// Define the function to convert a temperature from Fahrenheit to Celsius

function fahrenheitToCelsius($fahrenheit) {
  return ($fahrenheit - 32) * 5/9;
}

// Get the input from the user

$fahrenheit = readline('Enter the temperature in Fahrenheit: ');

// Convert the temperature to Celsius

$celsius = fahrenheitToCelsius($fahrenheit);

// Print the result

echo "The temperature in Celsius is $celsius";

// Calculate the BMI of a person

// Define the function to calculate the BMI of a person

function bmi($weight, $height) {
  return $weight / ($height * $height);
}

// Get the input from the user

$weight = readline('Enter your weight in kilograms: ');
$height = readline('Enter your height in meters: ');

// Calculate the BMI of the person

$bmi = bmi($weight, $height);

// Print the result

echo "Your BMI is $bmi";

// Calculate the surface area of a cube

// Define the function to calculate the surface area of a cube

function surfaceAreaOfCube($sideLength) {
  return 6 * $sideLength**2;
}

// Get the input from the user

$sideLength = readline('Enter the side length of the cube: ');

// Calculate the surface area of the cube

$surfaceArea = surfaceAreaOfCube($sideLength);

// Print the result

echo "The surface area of the cube is $surfaceArea";

```