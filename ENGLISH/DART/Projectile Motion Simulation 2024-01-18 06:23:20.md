```dart
import 'dart:math' as math;

// Define a class to calculate speed and distance
class Motion {
  // Properties to store initial velocity and acceleration
  double initialVelocity, acceleration;

  // Constructor to initialize the properties
  Motion(this.initialVelocity, this.acceleration);

  // Method to calculate distance traveled
  double calculateDistance(double time) =>
      initialVelocity * time + 0.5 * acceleration * time * time;

  // Method to calculate speed at a given time
  double calculateSpeed(double time) =>
      initialVelocity + acceleration * time;
}

// Define a class to represent a projectile
class Projectile {
  // Properties to store initial velocity, angle, and position
  double initialVelocity, angle, xPosition, yPosition;

  // Constructor to initialize the properties
  Projectile(this.initialVelocity, this.angle, this.xPosition, this.yPosition);

  // Method to calculate the horizontal distance traveled
  double calculateHorizontalDistance() => initialVelocity *
      math.cos(angle * math.pi / 180) *
      math.sqrt(2 * yPosition / acceleration);

  // Method to calculate the maximum height reached
  double calculateMaximumHeight() => initialVelocity *
      initialVelocity *
      math.sin(angle * math.pi / 180) *
      math.sin(angle * math.pi / 180) /
      (2 * acceleration);

  // Method to calculate the time taken to reach the maximum height
  double calculateTimeToReachMaximumHeight() =>
      initialVelocity * math.sin(angle * math.pi / 180) / acceleration;

  // Method to calculate the time taken to hit the ground
  double calculateTimeToHitGround() => 2 *
      calculateTimeToReachMaximumHeight(); // Symmetric trajectory

  // Method to calculate the trajectory of the projectile
  List<List<double>> calculateTrajectory(int numPoints) {
    // Initialize the trajectory list
    var trajectory = <List<double>>[];

    // Calculate the time interval between points
    var timeInterval = calculateTimeToHitGround() / (numPoints - 1);

    // Calculate the trajectory points
    for (var i = 0; i < numPoints; i++) {
      // Calculate the time at this point
      var time = i * timeInterval;

      // Calculate the x and y positions at this point
      var x = xPosition + initialVelocity *
          math.cos(angle * math.pi / 180) *
          time;
      var y = yPosition + initialVelocity *
          math.sin(angle * math.pi / 180) *
          time -
          0.5 * acceleration * time * time;

      // Add the point to the trajectory list
      trajectory.add([x, y]);
    }

    // Return the trajectory list
    return trajectory;
  }
}

// Main function to test the classes
void main() {
  // Create an instance of the Motion class
  var motion = Motion(10.0, 9.8);

  // Calculate the distance traveled after 5 seconds
  var distance = motion.calculateDistance(5.0);

  // Calculate the speed after 5 seconds
  var speed = motion.calculateSpeed(5.0);

  // Print the results
  print('Distance traveled after 5 seconds: $distance meters');
  print('Speed after 5 seconds: $speed m/s');

  // Create an instance of the Projectile class
  var projectile = Projectile(10.0, 45.0, 0.0, 0.0);

  // Calculate the horizontal distance traveled
  var horizontalDistance = projectile.calculateHorizontalDistance();

  // Calculate the maximum height reached
  var maximumHeight = projectile.calculateMaximumHeight();

  // Calculate the time taken to reach the maximum height
  var timeToReachMaximumHeight =
      projectile.calculateTimeToReachMaximumHeight();

  // Calculate the time taken to hit the ground
  var timeToHitGround = projectile.calculateTimeToHitGround();

  // Print the results
  print('Horizontal distance traveled: $horizontalDistance meters');
  print('Maximum height reached: $maximumHeight meters');
  print('Time taken to reach maximum height: $timeToReachMaximumHeight seconds');
  print('Time taken to hit the ground: $timeToHitGround seconds');

  // Calculate the trajectory of the projectile with 20 points
  var trajectory = projectile.calculateTrajectory(20);

  // Print the trajectory points
  for (var point in trajectory) {
    print('($point[0], $point[1])');
  }
}
```

Explanation:

1. `Motion` class:
   - Defines properties for initial velocity and acceleration.
   - Calculates distance and speed at a given time.

2. `Projectile` class:
   - Defines properties for initial velocity, angle, and position.
   - Calculates horizontal distance, maximum height, and time to reach maximum height and hit the ground.
   - Calculates the trajectory of the projectile with a specified number of points.

3. `main` function:
   - Creates instances of `Motion` and `Projectile` classes.
   - Calculates and prints the distance traveled, speed, horizontal distance, maximum height, time to reach maximum height, time to hit the ground, and the trajectory of the projectile.

This code demonstrates the use of classes, math functions, and loops to perform complex calculations related to projectile motion.