```kotlin
// Import necessary libraries
import java.util.*
import kotlin.math.*

// Define a data class to represent a point in 3D space
data class Point3D(val x: Double, val y: Double, val z: Double) {

    // Define a companion object to provide factory methods
    companion object {
        // Factory method to create a point from its coordinates
        fun fromCoordinates(x: Double, y: Double, z: Double): Point3D {
            return Point3D(x, y, z)
        }

        // Factory method to create a point from a list of coordinates
        fun fromList(coordinates: List<Double>): Point3D {
            require(coordinates.size == 3) { "Expected 3 coordinates, but got ${coordinates.size}" }
            return Point3D(coordinates[0], coordinates[1], coordinates[2])
        }
    }

    // Define a method to calculate the distance between two points
    fun distanceTo(other: Point3D): Double {
        val dx = this.x - other.x
        val dy = this.y - other.y
        val dz = this.z - other.z
        return sqrt(dx * dx + dy * dy + dz * dz)
    }

    // Define a method to calculate the midpoint between two points
    fun midpointTo(other: Point3D): Point3D {
        return Point3D((this.x + other.x) / 2, (this.y + other.y) / 2, (this.z + other.z) / 2)
    }

    // Override the toString() method to provide a human-readable representation of the point
    override fun toString(): String {
        return "($x, $y, $z)"
    }
}

// Define a function to generate a random point in 3D space
fun generateRandomPoint3D(min: Double, max: Double): Point3D {
    val random = Random()
    return Point3D(
        min + random.nextDouble() * (max - min),
        min + random.nextDouble() * (max - min),
        min + random.nextDouble() * (max - min)
    )
}

// Define a function to calculate the distance between two points in a list of points
fun distanceBetweenPoints(points: List<Point3D>, i: Int, j: Int): Double {
    return points[i].distanceTo(points[j])
}

// Define a function to find the closest pair of points in a list of points
fun findClosestPair(points: List<Point3D>): Pair<Point3D, Point3D> {
    var closestPair = Pair(points[0], points[1])
    var closestDistance = distanceBetweenPoints(points, 0, 1)

    for (i in 0 until points.size) {
        for (j in i + 1 until points.size) {
            val distance = distanceBetweenPoints(points, i, j)
            if (distance < closestDistance) {
                closestPair = Pair(points[i], points[j])
                closestDistance = distance
            }
        }
    }

    return closestPair
}

// Define a function to test the code
fun main() {
    // Generate a list of 10 random points in 3D space
    val points = (1..10).map { generateRandomPoint3D(-10.0, 10.0) }

    // Find the closest pair of points
    val closestPair = findClosestPair(points)

    // Print the closest pair of points and their distance
    println("Closest pair: $closestPair")
    println("Distance: ${closestPair.first.distanceTo(closestPair.second)}")
}
```

**Explanation:**

This code performs a brute-force algorithm to find the closest pair of points in a list of points in 3D space.

1. It starts by defining a data class called `Point3D` to represent a point in 3D space. This class has methods to calculate the distance between two points, the midpoint between two points, and to provide a human-readable representation of the point.
2. Next, it defines a function called `generateRandomPoint3D` to generate a random point in 3D space within a specified range.
3. It then defines a function called `distanceBetweenPoints` to calculate the distance between two points in a list of points.
4. The `findClosestPair` function is the core of the algorithm. It takes a list of points as input and finds the closest pair of points using a brute-force approach. It does this by iterating over all pairs of points in the list and calculating the distance between them. The pair of points with the smallest distance is the closest pair.
5. Finally, the `main` function is the entry point of the program. It generates a list of 10 random points in 3D space, calls the `findClosestPair` function to find the closest pair of points, and prints the closest pair of points and their distance to the console.

This code demonstrates a complex and differentiated approach to finding the closest pair of points in a list of points in 3D space, using a brute-force algorithm. It is a good example of how to use Kotlin to solve a complex problem in a clear and concise manner.