Claro! Aqui está um código complexo em Go que implementa um algoritmo de busca A* (A star) em um mapa bidimensional:

```go
package main

import (
	"fmt"
	"math"
)

type Node struct {
	x, y     int
	g, h, f  float64
	parent   *Node
}

type Mapa [][]int

func AStar(mapa Mapa, start, goal [2]int) []Node {
	openList := make([]Node, 0)
	closedList := make(map[[2]int]bool)

	startNode := Node{x: start[0], y: start[1]}
	goalNode := Node{x: goal[0], y: goal[1]}

	openList = append(openList, startNode)

	for len(openList) > 0 {
		currentNode := openList[0]
		currentIndex := 0

		for i, node := range openList {
			if node.f < currentNode.f {
				currentNode = node
				currentIndex = i
			}
		}

		openList = append(openList[:currentIndex], openList[currentIndex+1:]...)
		closedList[[2]int{currentNode.x, currentNode.y}] = true

		if currentNode.x == goalNode.x && currentNode.y == goalNode.y {
			path := make([]Node, 0)
			current := currentNode

			for current != startNode {
				path = append(path, current)
				current = *current.parent
			}

			return path
		}

		adjacentNodes := getAdjacentNodes(mapa, currentNode)

		for _, adjacentNode := range adjacentNodes {
			if closedList[[2]int{adjacentNode.x, adjacentNode.y}] {
				continue
			}

			newG := currentNode.g + heuristic(currentNode, adjacentNode)

			if newG < adjacentNode.g || !contains(openList, adjacentNode) {
				adjacentNode.g = newG
				adjacentNode.h = heuristic(adjacentNode, goalNode)
				adjacentNode.f = adjacentNode.g + adjacentNode.h
				adjacentNode.parent = &currentNode

				if !contains(openList, adjacentNode) {
					openList = append(openList, adjacentNode)
				}
			}
		}
	}

	return nil
}

func getAdjacentNodes(mapa Mapa, node Node) []Node {
	directions := [4][2]int{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
	adjacentNodes := make([]Node, 0)

	for _, direction := range directions {
		newX := node.x + direction[0]
		newY := node.y + direction[1]

		if newX >= 0 && newX < len(mapa) && newY >= 0 && newY < len(mapa[0]) && mapa[newX][newY] != 1 {
			adjacentNodes = append(adjacentNodes, Node{x: newX, y: newY})
		}
	}

	return adjacentNodes
}

func heuristic(node1, node2 Node) float64 {
	dx := math.Abs(float64(node1.x - node2.x))
	dy := math.Abs(float64(node1.y - node2.y))
	return dx + dy
}

func contains(nodes []Node, node Node) bool {
	for _, n := range nodes {
		if n.x == node.x && n.y == node.y {
			return true
		}
	}
	return false
}

func main() {
	mapa := Mapa{
		{0, 0, 0, 0, 0},
		{0, 1, 1, 1, 0},
		{0, 0, 0, 0, 0},
		{0, 1, 1, 1, 0},
		{0, 0, 0, 0, 0},
	}

	start := [2]int{0, 0}
	goal := [2]int{4, 4}

	path := AStar(mapa, start, goal)

	if path != nil {
		fmt.Println("Caminho encontrado:")
		for i := len(path) - 1; i >= 0; i-- {
			fmt.Printf("(%d, %d)\n", path[i].x, path[i].y)
		}
	} else {
		fmt.Println("Caminho não encontrado!")
	}
}
```

Neste código, a função `AStar` implementa o algoritmo de busca A* para encontrar o caminho mais curto entre dois pontos em um mapa bidimensional. A função `getAdjacentNodes` retorna os nós adjacentes a um determinado nó dentro do mapa. A função `heuristic` calcula a heurística utilizada pelo algoritmo A*. A função `contains` verifica se um nó está presente em uma lista de nós.

No `main`, é criado um mapa de exemplo e são definidos os pontos de início e fim. O caminho resultante é impresso na saída.

Espero que esse código atenda às suas expectativas!