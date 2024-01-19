```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

const (
	gridSizeX = 100
	gridSizeY = 100
	numGenerations = 100
)

type cell struct {
	state bool
}

type grid [][]cell

func (g grid) String() string {
	var s string
	for _, row := range g {
		for _, c := range row {
			if c.state {
				s += "#"
			} else {
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

func newGrid() grid {
	g := make(grid, gridSizeX)
	for i := 0; i < gridSizeX; i++ {
		g[i] = make([]cell, gridSizeY)
	}
	return g
}

func (g grid) countNeighbors(x, y int) int {
	var neighbors int
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i == 0 && j == 0 {
				continue
			}
			if x+i < 0 || x+i >= gridSizeX || y+j < 0 || y+j >= gridSizeY {
				continue
			}
			if g[x+i][y+j].state {
				neighbors++
			}
		}
	}
	return neighbors
}

func (g grid) nextGeneration() grid {
	nextGen := newGrid()
	for i := 0; i < gridSizeX; i++ {
		for j := 0; j < gridSizeY; j++ {
			neighbors := g.countNeighbors(i, j)
			if g[i][j].state {
				if neighbors < 2 || neighbors > 3 {
					nextGen[i][j].state = false
				} else {
					nextGen[i][j].state = true
				}
			} else {
				if neighbors == 3 {
					nextGen[i][j].state = true
				} else {
					nextGen[i][j].state = false
				}
			}
		}
	}
	return nextGen
}

func main() {
	rand.Seed(time.Now().UnixNano())
	g := newGrid()
	for i := 0; i < gridSizeX; i++ {
		for j := 0; j < gridSizeY; j++ {
			g[i][j].state = rand.Intn(2) == 1
		}
	}
	fmt.Println("Initial grid:")
	fmt.Println(g)
	for i := 0; i < numGenerations; i++ {
		g = g.nextGeneration()
		fmt.Printf("Generation %d:\n", i+1)
		fmt.Println(g)
		time.Sleep(time.Second / 10)
	}
}
```

This code simulates Conway's Game of Life, a cellular automaton invented by the British mathematician John Horton Conway in 1970. The game is a zero-player game, meaning that its evolution is determined by its initial state, without any further input from players.

The game is played on a two-dimensional grid of cells, each of which can be in one of two states: alive or dead. The rules of the game are as follows:

* Any live cell with fewer than two live neighbors dies of isolation.
* Any live cell with two or three live neighbors lives on to the next generation.
* Any live cell with four or more live neighbors dies of overcrowding.
* Any dead cell with exactly three live neighbors becomes a live cell.

The code starts by creating a new grid of cells, with each cell being randomly assigned a state of alive or dead. The initial grid is then printed to the console. The code then enters a loop that runs for a specified number of generations. In each generation, the code calculates the next state of each cell based on the rules of the game. The next generation is then printed to the console, and the loop repeats.

The output of the code is a series of grids, showing the evolution of the Game of Life over time. The patterns that emerge are often complex and fascinating, and the game has been used to explore a wide range of scientific and mathematical concepts, including self-organization, emergence, and complexity.