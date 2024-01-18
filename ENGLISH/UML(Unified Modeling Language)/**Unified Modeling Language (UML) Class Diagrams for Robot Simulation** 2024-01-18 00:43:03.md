```uml
+----------------------------------+
|                                  |
|                Robot              |
|                                  |
+----------------------------------+
|                                  |
|  - name : String                |
|  - position : Point             |
|  - orientation : Angle          |
|  - leftWheelSpeed : Decimal     |
|  - rightWheelSpeed : Decimal    |
|                                  |
|  + moveForward()                |
|  + moveBackward()               |
|  + turnLeft()                   |
|  + turnRight()                  |
|                                  |
+----------------------------------+

+----------------------------------+
|                                  |
|                Point             |
|                                  |
+----------------------------------+
|                                  |
|  - x : Decimal                  |
|  - y : Decimal                  |
|                                  |
|  + translate(x, y)             |
|                                  |
+----------------------------------+

+----------------------------------+
|                                  |
|                Angle             |
|                                  |
+----------------------------------+
|                                  |
|  - degrees : Decimal            |
|                                  |
|  + rotate(degrees)             |
|                                  |
+----------------------------------+

+----------------------------------+
|                                  |
|               World              |
|                                  |
+----------------------------------+
|                                  |
|  - robots : List[Robot]         |
|  - obstacles : List[Obstacle]  |
|                                  |
|  + addRobot(robot)             |
|  + addObstacle(obstacle)       |
|                                  |
+----------------------------------+

+----------------------------------+
|                                  |
|               Obstacle           |
|                                  |
+----------------------------------+
|                                  |
|  - position : Point             |
|  - radius : Decimal             |
|                                  |
|                                  |
+----------------------------------+

+----------------------------------+
|                                  |
|               Simulation         |
|                                  |
+----------------------------------+
|                                  |
|  - world : World                |
|  - time : Decimal               |
|                                  |
|  + update()                    |
|                                  |
+----------------------------------+

```

**Explanation:**

* The `Robot` class represents a robot in the world. It has a name, a position, an orientation, and the speeds of its left and right wheels. It also has methods to move forward, backward, turn left, and turn right.
* The `Point` class represents a point in two-dimensional space. It has x and y coordinates. It also has a method to translate the point by a given x and y offset.
* The `Angle` class represents an angle in degrees. It has a `degrees` attribute. It also has a method to rotate the angle by a given number of degrees.
* The `World` class represents the world in which the robots and obstacles exist. It has a list of robots and a list of obstacles. It also has methods to add robots and obstacles to the world.
* The `Obstacle` class represents an obstacle in the world. It has a position and a radius.
* The `Simulation` class represents the simulation of the world. It has a world and a time attribute. It also has a method to update the simulation by moving the robots and checking for collisions.

This code could be used to create a simulation of a world with robots and obstacles. The simulation could be used to test the behavior of different robot control algorithms.