import 'dart:math';

// Define a function to generate a random number between two values.
double generateRandomNumber(double min, double max) {
  final random = Random();
  return min + random.nextDouble() * (max - min);
}

// Define a function to calculate the area of a circle.
double calculateAreaOfCircle(double radius) {
  return pi * radius * radius;
}

// Define a function to calculate the volume of a sphere.
double calculateVolumeOfSphere(double radius) {
  return (4 / 3) * pi * radius * radius * radius;
}

// Define a function to calculate the surface area of a sphere.
double calculateSurfaceAreaOfSphere(double radius) {
  return 4 * pi * radius * radius;
}

// Define a function to convert degrees to radians.
double convertDegreesToRadians(double degrees) {
  return degrees * (pi / 180);
}

// Define a function to convert radians to degrees.
double convertRadiansToDegrees(double radians) {
  return radians * (180 / pi);
}

// Define a function to calculate the distance between two points in 2D space.
double calculateDistanceBetweenPoints(double x1, double y1, double x2, double y2) {
  return sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
}

// Define a function to calculate the slope of a line between two points.
double calculateSlopeOfLine(double x1, double y1, double x2, double y2) {
  return (y2 - y1) / (x2 - x1);
}

// Define a function to calculate the y-intercept of a line given a slope and a point.
double calculateYInterceptOfLine(double slope, double x, double y) {
  return y - slope * x;
}

void main() {
  // Generate a random number between 0 and 10.
  double randomNumber = generateRandomNumber(0, 10);
  print('Random number: $randomNumber');

  // Calculate the area of a circle with a radius of 5.
  double areaOfCircle = calculateAreaOfCircle(5);
  print('Area of circle: $areaOfCircle');

  // Calculate the volume of a sphere with a radius of 5.
  double volumeOfSphere = calculateVolumeOfSphere(5);
  print('Volume of sphere: $volumeOfSphere');

  // Calculate the surface area of a sphere with a radius of 5.
  double surfaceAreaOfSphere = calculateSurfaceAreaOfSphere(5);
  print('Surface area of sphere: $surfaceAreaOfSphere');

  // Convert 45 degrees to radians.
  double radians = convertDegreesToRadians(45);
  print('Radians: $radians');

  // Convert 1.57 radians to degrees.
  double degrees = convertRadiansToDegrees(1.57);
  print('Degrees: $degrees');

  // Calculate the distance between two points in 2D space.
  double distanceBetweenPoints = calculateDistanceBetweenPoints(0, 0, 5, 5);
  print('Distance between points: $distanceBetweenPoints');

  // Calculate the slope of a line between two points.
  double slopeOfLine = calculateSlopeOfLine(0, 0, 5, 5);
  print('Slope of line: $slopeOfLine');

  // Calculate the y-intercept of a line given a slope and a point.
  double yInterceptOfLine = calculateYInterceptOfLine(slopeOfLine, 0, 0);
  print('Y-intercept of line: $yInterceptOfLine');
}

Explanation:
This code demonstrates a variety of mathematical and geometric calculations in Dart. It includes functions for generating random numbers, calculating the area and volume of a circle and sphere, converting between degrees and radians, calculating the distance between points, and calculating the slope and y-intercept of a line. The main() function demonstrates how to use these functions.