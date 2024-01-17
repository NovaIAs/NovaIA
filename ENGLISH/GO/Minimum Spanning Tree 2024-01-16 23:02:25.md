```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

// Data structure to represent a point in 2D space.
type Point struct {
	x, y float64
}

// Data structure to represent a line segment in 2D space.
type Segment struct {
	p1, p2 Point
}

// Function to calculate the distance between two points.
func distance(p1, p2 Point) float64 {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	return math.Sqrt(dx*dx + dy*dy)
}

// Function to check if two line segments intersect.
func intersect(s1, s2 Segment) bool {
	// Check if the two segments are parallel.
	if (s1.p1.x-s1.p2.x)*(s2.p1.y-s2.p2.y) == (s1.p1.y-s1.p2.y)*(s2.p1.x-s2.p2.x) {
		return false
	}

	// Check if the two segments intersect on the x-axis.
	x1 := min(s1.p1.x, s1.p2.x)
	x2 := max(s1.p1.x, s1.p2.x)
	x3 := min(s2.p1.x, s2.p2.x)
	x4 := max(s2.p1.x, s2.p2.x)
	if x2 < x3 || x4 < x1 {
		return false
	}

	// Check if the two segments intersect on the y-axis.
	y1 := min(s1.p1.y, s1.p2.y)
	y2 := max(s1.p1.y, s1.p2.y)
	y3 := min(s2.p1.y, s2.p2.y)
	y4 := max(s2.p1.y, s2.p2.y)
	if y2 < y3 || y4 < y1 {
		return false
	}

	// Check if the two segments intersect at a point.
	det := (s1.p1.x-s1.p2.x)*(s2.p1.y-s2.p2.y) - (s1.p1.y-s1.p2.y)*(s2.p1.x-s2.p2.x)
	if det == 0 {
		return false
	}
	t1 := ((s1.p1.y-s2.p1.y)*(s2.p1.x-s2.p2.x) - (s1.p1.x-s2.p1.x)*(s2.p1.y-s2.p2.y)) / det
	t2 := ((s1.p1.y-s2.p1.y)*(s1.p1.x-s1.p2.x) - (s1.p1.x-s2.p1.x)*(s1.p1.y-s1.p2.y)) / det
	if t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1 {
		return false
	}

	return true
}

// Function to find the minimum distance between two line segments.
func minDistance(s1, s2 Segment) float64 {
	// Check if the two segments intersect.
	if intersect(s1, s2) {
		return 0
	}

	// Find the closest point on s1 to s2.
	closestPoint := Point{0, 0}
	minDist := math.Inf(1)
	for i := 0; i <= 1; i++ {
		for j := 0; j <= 1; j++ {
			p := Point{s1.p1.x + i*(s1.p2.x-s1.p1.x), s1.p1.y + j*(s1.p2.y-s1.p1.y)}
			dist := distance(p, s2.p1)
			if dist < minDist {
				minDist = dist
				closestPoint = p
			}
			dist = distance(p, s2.p2)
			if dist < minDist {
				minDist = dist
				closestPoint = p
			}
		}
	}

	// Find the closest point on s2 to s1.
	for i := 0; i <= 1; i++ {
		for j := 0; j <= 1; j++ {
			p := Point{s2.p1.x + i*(s2.p2.x-s2.p1.x), s2.p1.y + j*(s2.p2.y-s2.p1.y)}
			dist := distance(p, s1.p1)
			if dist < minDist {
				minDist = dist
				closestPoint = p
			}
			dist = distance(p, s1.p2)
			if dist < minDist {
				minDist = dist
				closestPoint = p
			}
		}
	}

	return minDist
}

// Function to find the minimum spanning tree of a graph.
func mst(graph map[Point][]Point) map[Point]Point {
	// Initialize the MST.
	mst := make(map[Point]Point)

	// Initialize the set of visited nodes.
	visited := make(map[Point]bool)

	// Start at an arbitrary node.
	var start Point
	for point := range graph {
		start = point
		break
	}

	// Visit the starting node.
	visited[start] = true

	// While there are still unvisited nodes,
	for len(visited) < len(graph) {
		// Find the closest unvisited node to the current node.
		closestNode := Point{0, 0}
		minDist := math.Inf(1)
		for point, edges := range graph {
			if !visited[point] {
				for _, edge := range edges {
					if visited[edge] {
						dist := distance(point, edge)
						if dist < minDist {
							minDist = dist
							closestNode = point
						}
					}
				}
			}
		}

		// Add the closest node to the MST.
		mst[start] = closestNode
		visited[closestNode] = true

		// Start at the closest node.
		start = closestNode
	}

	return mst
}

// Function to read a graph from a file.
func readGraph(filename string) map[Point][]Point {
	// Open the file.
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read the number of nodes and edges.
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	n, _ := strconv.Atoi(scanner.Text())
	scanner.Scan()
	m, _ := strconv.Atoi(scanner.Text())

	// Read the nodes.
	nodes := make([]Point, n)
	for i := 0; i < n; i++ {
		scanner.Scan()
		line := strings.Split(scanner.Text(), " ")
		x, _ := strconv.ParseFloat(line[0], 64)
		y, _ := strconv.ParseFloat(line[1], 64)
		nodes[i] = Point{x, y}
	}

	// Read the edges.
	edges := make([]Segment, m)
	for i := 0; i < m; i++ {
		scanner.Scan()
		line := strings.Split(scanner.Text(), " ")
		u, _ := strconv.Atoi(line[0])
		v, _ := strconv.Atoi(line[1])
		edges[i] = Segment{nodes[u], nodes[v]}
	}

	// Create the graph.
	graph := make(map[Point][]Point)
	for _, edge := range edges {
		graph[edge.p1] = append(graph[edge.p1], edge.p2)
		graph[edge.p2] = append(graph[edge.p2], edge.p1)
	}

	return graph
}

// Function to write the MST to a file.
func writeMst(filename