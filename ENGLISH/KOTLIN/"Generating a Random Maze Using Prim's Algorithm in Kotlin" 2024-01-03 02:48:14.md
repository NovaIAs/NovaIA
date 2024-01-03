Certainly! Here's a complex and unique Kotlin code that generates a random maze using the Prim's algorithm. This algorithm starts with a grid of walls and gradually connects adjacent cells to form a maze.

```kotlin
import java.util.*

class MazeGenerator(private val rows: Int, private val cols: Int) {
    private val grid: Array<Array<Cell>>
    private val random: Random = Random()

    init {
        grid = Array(rows) { r ->
            Array(cols) { c ->
                Cell(r, c)
            }
        }
    }

    inner class Cell(val row: Int, val col: Int) {
        var visited: Boolean = false
        var topWall: Boolean = true
        var rightWall: Boolean = true
        var bottomWall: Boolean = true
        var leftWall: Boolean = true
    }

    fun generateMaze(): Array<Array<Boolean>> {
        val startCell = grid[0][0]
        startCell.visited = true

        val stack = Stack<Cell>()
        stack.push(startCell)

        while (!stack.empty()) {
            val currentCell = stack.pop()
            val neighbors = getUnvisitedNeighbors(currentCell)
            if (neighbors.isNotEmpty()) {
                stack.push(currentCell)

                val nextCell = neighbors[random.nextInt(neighbors.size)]
                removeWall(currentCell, nextCell)
                nextCell.visited = true
                stack.push(nextCell)
            }
        }

        return convertToBooleanArray()
    }

    private fun getUnvisitedNeighbors(cell: Cell): List<Cell> {
        val neighbors = mutableListOf<Cell>()

        if (cell.row > 0 && !grid[cell.row - 1][cell.col].visited) {
            neighbors.add(grid[cell.row - 1][cell.col])
        }
        if (cell.col < cols - 1 && !grid[cell.row][cell.col + 1].visited) {
            neighbors.add(grid[cell.row][cell.col + 1])
        }
        if (cell.row < rows - 1 && !grid[cell.row + 1][cell.col].visited) {
            neighbors.add(grid[cell.row + 1][cell.col])
        }
        if (cell.col > 0 && !grid[cell.row][cell.col - 1].visited) {
            neighbors.add(grid[cell.row][cell.col - 1])
        }

        return neighbors
    }

    private fun removeWall(currentCell: Cell, nextCell: Cell) {
        when {
            currentCell.row > nextCell.row -> {
                currentCell.topWall = false
                nextCell.bottomWall = false
            }
            currentCell.col < nextCell.col -> {
                currentCell.rightWall = false
                nextCell.leftWall = false
            }
            currentCell.row < nextCell.row -> {
                currentCell.bottomWall = false
                nextCell.topWall = false
            }
            currentCell.col > nextCell.col -> {
                currentCell.leftWall = false
                nextCell.rightWall = false
            }
        }
    }

    private fun convertToBooleanArray(): Array<Array<Boolean>> {
        val maze = Array(2 * rows + 1) { BooleanArray(2 * cols + 1) }

        for (r in 0 until rows) {
            for (c in 0 until cols) {
                val cell = grid[r][c]

                maze[2 * r + 1][2 * c + 1] = true

                if (cell.topWall) {
                    maze[2 * r][2 * c + 1] = true
                }
                if (cell.rightWall) {
                    maze[2 * r + 1][2 * c + 2] = true
                }
                if (cell.bottomWall) {
                    maze[2 * r + 2][2 * c + 1] = true
                }
                if (cell.leftWall) {
                    maze[2 * r + 1][2 * c] = true
                }
            }
        }

        return maze.map { it.toTypedArray() }.toTypedArray()
    }
}

fun main() {
    val rows = 10
    val cols = 10
    val mazeGenerator = MazeGenerator(rows, cols)
    val maze = mazeGenerator.generateMaze()

    for (row in maze) {
        for (cell in row) {
            print(if (cell) " " else "#")
        }
        println()
    }
}
```

This code implements a maze generator using the Prim's algorithm in Kotlin. The `MazeGenerator` class creates a grid of cells, where each cell is a unit of the maze. The algorithm starts at the top-left cell and randomly chooses neighboring cells to connect, removing walls in the process. The algorithm repeats until all cells have been visited.

The generated maze is represented as a 2D boolean array, where `true` represents a path and `false` represents a wall. The `generateMaze()` function returns this boolean array, and the `main()` function prints the maze to the console using spaces for paths and "#" for walls.

You can adjust the `rows` and `cols` variables in the `main()` function to generate mazes of different sizes.