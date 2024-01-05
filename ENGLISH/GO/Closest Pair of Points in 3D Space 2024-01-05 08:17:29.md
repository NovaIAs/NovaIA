```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Define a struct to represent a point in 3D space.
type Point struct {
	X, Y, Z float64
}

// Define a function to generate a random point in a given range.
func randomPoint(min, max float64) Point {
	return Point{
		X: min + rand.Float64()*(max-min),
		Y: min + rand.Float64()*(max-min),
		Z: min + rand.Float64()*(max-min),
	}
}

// Define a function to calculate the distance between two points.
func distance(p1, p2 Point) float64 {
	dx := p2.X - p1.X
	dy := p2.Y - p1.Y
	dz := p2.Z - p1.Z
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

// Define a function to find the closest pair of points in a set of points.
func closestPair(points []Point) (Point, Point, float64) {
	// Initialize the closest pair of points and the closest distance.
	closestPair := Point{}, Point{}
	closestDistance := math.MaxFloat64

	// Iterate over all pairs of points.
	for i := 0; i < len(points); i++ {
		for j := i + 1; j < len(points); j++ {
			// Calculate the distance between the two points.
			dist := distance(points[i], points[j])

			// If the distance is less than the closest distance, update the closest pair of points and the closest distance.
			if dist < closestDistance {
				closestPair = points[i], points[j]
				closestDistance = dist
			}
		}
	}

	// Return the closest pair of points and the closest distance.
	return closestPair, closestDistance
}

func main() {
	// Set the random seed.
	rand.Seed(time.Now().UnixNano())

	// Generate a set of 100 random points in the range [0, 100].
	points := make([]Point, 100)
	for i := 0; i < 100; i++ {
		points[i] = randomPoint(0, 100)
	}

	// Find the closest pair of points in the set of points.
	closestPair, closestDistance := closestPair(points)

	// Print the closest pair of points and the closest distance.
	fmt.Println("Closest pair of points:", closestPair)
	fmt.Println("Closest distance:", closestDistance)
}
```

This code defines a struct to represent a point in 3D space, a function to generate a random point in a given range, a function to calculate the distance between two points, and a function to find the closest pair of points in a set of points. The main function generates a set of 100 random points in the range [0, 100], finds the closest pair of points in the set of points, and prints the closest pair of points and the closest distance.