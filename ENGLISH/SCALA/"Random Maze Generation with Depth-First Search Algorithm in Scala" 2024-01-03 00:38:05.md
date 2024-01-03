Certainly! Here's a complex Scala code that generates a random maze using a depth-first search algorithm.

```scala
import scala.collection.mutable.Stack
import scala.util.Random

object MazeGenerator {
  type Maze = Array[Array[Boolean]]

  def generateMaze(width: Int, height: Int): Maze = {
    val maze = Array.ofDim[Boolean](height, width)
    val stack = Stack[(Int, Int)]()
    val random = new Random()

    def isInside(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
    def isUnvisited(x: Int, y: Int): Boolean = isInside(x, y) && !maze(y)(x)

    def getUnvisitedNeighbors(x: Int, y: Int): List[(Int, Int)] = {
      val neighbors = List((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))
      neighbors.filter(n => isUnvisited(n._1, n._2))
    }

    def removeWall(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
      maze(y1)(x1) = true
      maze(y2)(x2) = true
    }

    def generate(x: Int, y: Int): Unit = {
      maze(y)(x) = true
      stack.push((x, y))

      while (stack.nonEmpty) {
        val (curX, curY) = stack.pop()
        val neighbors = getUnvisitedNeighbors(curX, curY)

        if (neighbors.nonEmpty) {
          stack.push((curX, curY))

          val randomNeighbor = neighbors(random.nextInt(neighbors.length))
          val (nextX, nextY) = randomNeighbor
          removeWall(curX, curY, nextX, nextY)
          stack.push((nextX, nextY))
        }
      }
    }

    generate(random.nextInt(width), random.nextInt(height))
    maze
  }

  def printMaze(maze: Maze): Unit = {
    for (row <- maze) {
      for (cell <- row) {
        if (cell) print("  ") else print("██")
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val mazeWidth = 15
    val mazeHeight = 10
    val maze = generateMaze(mazeWidth, mazeHeight)
    printMaze(maze)
  }
}
```

This code generates a random maze using a depth-first search algorithm. The `generateMaze` function takes in the width and height of the maze and returns a 2D array representing the maze.

The algorithm starts at a random position in the maze and uses a stack to keep track of visited cells. It then repeatedly chooses a random unvisited neighbor of the current cell, removes the wall between them, and moves to the neighbor. This process continues until there are no unvisited neighbors left.

The `printMaze` function prints the maze to the console, using spaces for open paths and solid blocks for walls.

In the `main` function, we specify the width and height of the maze (15 and 10, respectively) and generate the maze using `generateMaze`. Finally, we print the generated maze using `printMaze`.