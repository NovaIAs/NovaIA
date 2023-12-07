Claro! Aqui está um código em Go que gera um mapa aleatório utilizando a técnica de Geração de Labirinto Prim:

```
package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Cell struct {
	row, col int
}

type Direction int

const (
	UP    Direction = 0
	DOWN  Direction = 1
	LEFT  Direction = 2
	RIGHT Direction = 3
)

var (
	rows       int
	cols       int
	grid       [][]bool
	visited    [][]bool
	directions = []Direction{UP, DOWN, LEFT, RIGHT}
)

func main() {
	rand.Seed(time.Now().UnixNano())

	rows, cols = 10, 20
	initializeGrid()
	generateMaze()
	printMaze()
}

func initializeGrid() {
	grid = make([][]bool, rows)
	visited = make([][]bool, rows)

	for i := 0; i < rows; i++ {
		grid[i] = make([]bool, cols)
		visited[i] = make([]bool, cols)
	}
}

func generateMaze() {
	stack := []Cell{{0, 0}}
	visited[0][0] = true

	for len(stack) > 0 {
		current := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		nextCell := getRandomUnvisitedNeighbor(current)
		if nextCell.row != -1 {
			stack = append(stack, current)
			removeWall(current, nextCell)
			visited[nextCell.row][nextCell.col] = true
			stack = append(stack, nextCell)
		}
	}
}

func getRandomUnvisitedNeighbor(cell Cell) Cell {
	neighbors := make([]Cell, 0)

	for _, direction := range directions {
		row, col := cell.row, cell.col
		switch direction {
		case UP:
			row--
		case DOWN:
			row++
		case LEFT:
			col--
		case RIGHT:
			col++
		}

		if isWithinBounds(row, col) && !visited[row][col] {
			neighbors = append(neighbors, Cell{row, col})
		}
	}

	if len(neighbors) > 0 {
		return neighbors[rand.Intn(len(neighbors))]
	}

	return Cell{-1, -1}
}

func isWithinBounds(row, col int) bool {
	return row >= 0 && row < rows && col >= 0 && col < cols
}

func removeWall(current, next Cell) {
	rowDiff := next.row - current.row
	colDiff := next.col - current.col

	if rowDiff == -1 {
		grid[current.row][current.col] = true // Remove a parede superior
	} else if rowDiff == 1 {
		grid[next.row][next.col] = true // Remove a parede inferior
	} else if colDiff == -1 {
		grid[current.row][current.col] = true // Remove a parede esquerda
	} else if colDiff == 1 {
		grid[next.row][next.col] = true // Remove a parede direita
	}
}

func printMaze() {
	fmt.Print("┏")
	for i := 0; i < cols; i++ {
		fmt.Print("━━")
	}
	fmt.Println("┓")

	for i := 0; i < rows; i++ {
		fmt.Print("┃")
		for j := 0; j < cols; j++ {
			if grid[i][j] {
				fmt.Print("  ") // Espaço vazio
			} else {
				fmt.Print("██") // Parede
			}
		}
		fmt.Println("┃")
	}

	fmt.Print("┗")
	for i := 0; i < cols; i++ {
		fmt.Print("━━")
	}
	fmt.Println("┛")
}
```

Neste código, utilizamos a técnica de geração de labirintos Prim para criar um labirinto aleatório. A geração é feita a partir de um grid bidimensional composto por células. 

Inicialmente, definimos as estruturas `Cell` e `Direction`. Em seguida, declaramos variáveis globais para armazenar o número de linhas e colunas do grid, a matriz `grid` que representa as paredes do labirinto, a matriz `visited` que indica quais células já foram visitadas, e uma lista de direções possíveis.

No `main()`, inicializamos o grid e geramos o labirinto chamando a função `generateMaze()`. Por fim, imprimimos o labirinto na tela chamando `printMaze()`.

A função `generateMaze()` implementa o algoritmo de geração de labirinto Prim. Utilizamos uma pilha para armazenar as células visitadas. Começamos pela célula inicial (0, 0) e, em cada passo, escolhemos uma célula vizinha não visitada aleatoriamente para avançar. Essa célula vizinha é removida da pilha, removemos a parede entre a célula atual e a célula vizinha e marcamos a célula vizinha como visitada. Esse processo se repete até que não haja mais células vizinhas não visitadas disponíveis.

A função `getRandomUnvisitedNeighbor()` retorna uma célula vizinha não visitada aleatoriamente a partir da célula atual. A função `isWithinBounds()` verifica se uma célula está dentro dos limites do grid. A função `removeWall()` remove a parede entre a célula atual e a célula vizinha.

Por fim, a função `printMaze()` imprime o labirinto no console. Utilizamos caracteres Unicode para representar as paredes e espaços vazios.