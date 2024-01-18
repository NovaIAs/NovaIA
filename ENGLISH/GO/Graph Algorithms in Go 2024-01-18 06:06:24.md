```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// A data structure to represent a graph
type Graph struct {
	vertices []*Vertex
	edges    map[Vertex]map[Vertex]int // Weighted edges
}

// A data structure to represent a vertex in a graph
type Vertex struct {
	id    int
	label string
}

// AddVertex adds a new vertex to the graph
func (g *Graph) AddVertex(l string) *Vertex {
	newVertex := &Vertex{id: len(g.vertices), label: l}
	g.vertices = append(g.vertices, newVertex)
	g.edges[newVertex] = make(map[Vertex]int)
	return newVertex
}

// AddEdge adds a new edge between two vertices in the graph
func (g *Graph) AddEdge(from, to *Vertex, weight int) {
	g.edges[from][*to] = weight
	g.edges[to][*from] = weight
}

// Print prints the graph in a human-readable format
func (g *Graph) Print() {
	fmt.Println("Vertices:")
	for _, v := range g.vertices {
		fmt.Printf("%d: %s\n", v.id, v.label)
	}

	fmt.Println("Edges:")
	for from, m := range g.edges {
		for to, weight := range m {
			fmt.Printf("%d-%d: %d\n", from.id, to.id, weight)
		}
	}
}

// GenerateRandomGraph generates a random graph with n vertices and m edges
func GenerateRandomGraph(n, m int) *Graph {
	g := &Graph{
		vertices: make([]*Vertex, 0, n),
		edges:    make(map[Vertex]map[Vertex]int),
	}

	// Add n vertices to the graph
	for i := 0; i < n; i++ {
		g.AddVertex(fmt.Sprintf("v%d", i))
	}

	// Add m edges to the graph
	rand.Seed(time.Now().UnixNano())
	for i := 0; i < m; i++ {
		from := g.vertices[rand.Intn(n)]
		to := g.vertices[rand.Intn(n)]
		weight := rand.Intn(100)
		g.AddEdge(from, to, weight)
	}

	return g
}

// DepthFirstSearch performs a depth-first search of the graph starting from the given vertex
func (g *Graph) DepthFirstSearch(start *Vertex) {
	visited := make(map[*Vertex]bool)
	g.dfs(start, visited)
}

// dfs is a recursive function that performs a depth-first search of the graph
func (g *Graph) dfs(current *Vertex, visited map[*Vertex]bool) {
	// Mark the current vertex as visited
	visited[current] = true

	// Print the current vertex
	fmt.Printf("%s ", current.label)

	// Visit all unvisited neighbors of the current vertex
	for neighbor, _ := range g.edges[current] {
		if !visited[neighbor] {
			g.dfs(neighbor, visited)
		}
	}
}

// BreadthFirstSearch performs a breadth-first search of the graph starting from the given vertex
func (g *Graph) BreadthFirstSearch(start *Vertex) {
	visited := make(map[*Vertex]bool)
	queue := []*Vertex{start}
	visited[start] = true

	// While the queue is not empty
	for len(queue) > 0 {
		// Dequeue the first vertex from the queue
		current := queue[0]
		queue = queue[1:]

		// Print the current vertex
		fmt.Printf("%s ", current.label)

		// Visit all unvisited neighbors of the current vertex
		for neighbor, _ := range g.edges[current] {
			if !visited[neighbor] {
				queue = append(queue, neighbor)
				visited[neighbor] = true
			}
		}
	}
}

// Dijkstra's algorithm finds the shortest path from the given vertex to all other vertices in the graph
func (g *Graph) Dijkstra(start *Vertex) {
	// Initialize distances to infinity for all vertices
	distances := make(map[*Vertex]int)
	for _, v := range g.vertices {
		distances[v] = 1 << 31 // Maximum possible integer value
	}

	// Set the distance from the start vertex to itself to 0
	distances[start] = 0

	// Initialize the priority queue with the start vertex
	pq := NewPriorityQueue()
	pq.Push(start, 0)

	// While the priority queue is not empty
	for pq.Len() > 0 {
		// Get the vertex with the smallest distance from the priority queue
		current := pq.Pop()

		// Visit all unvisited neighbors of the current vertex
		for neighbor, weight := range g.edges[current] {
			// Calculate the new distance to the neighbor
			newDistance := distances[current] + weight

			// If the new distance is shorter than the current distance, update the distance and add the neighbor to the priority queue
			if newDistance < distances[neighbor] {
				distances[neighbor] = newDistance
				pq.Push(neighbor, newDistance)
			}
		}
	}

	// Print the shortest distances to all other vertices
	for _, v := range g.vertices {
		fmt.Printf("Shortest distance from %s to %s: %d\n", start.label, v.label, distances[v])
	}
}

// A priority queue data structure
type PriorityQueue struct {
	items []*Item
}

// An item in the priority queue
type Item struct {
	value    *Vertex
	priority int
}

// NewPriorityQueue creates a new priority queue
func NewPriorityQueue() *PriorityQueue {
	return &PriorityQueue{
		items: make([]*Item, 0),
	}
}

// Len returns the number of items in the priority queue
func (pq *PriorityQueue) Len() int {
	return len(pq.items)
}

// Push adds an item to the priority queue
func (pq *PriorityQueue) Push(value *Vertex, priority int) {
	item := &Item{
		value:    value,
		priority: priority,
	}

	// Add the item to the end of the queue
	pq.items = append(pq.items, item)

	// Heapify the queue
	pq.heapifyUp(len(pq.items) - 1)
}

// Pop removes and returns the item with the highest priority from the priority queue
func (pq *PriorityQueue) Pop() *Vertex {
	// Get the item with the highest priority
	item := pq.items[0]

	// Move the last item in the queue to the first position
	pq.items[0] = pq.items[len(pq.items)-1]
	pq.items = pq.items[:len(pq.items)-1]

	// Heapify the queue
	pq.heapifyDown(0)

	return item.value
}

// heapifyUp moves an item up the heap until it reaches its correct position
func (pq *PriorityQueue) heapifyUp(