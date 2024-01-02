Certainly! Here's a complex PHP code that generates a random maze using a depth-first search algorithm:

```php
<?php

class MazeGenerator
{
    private $width;
    private $height;
    private $maze;
    private $visited;

    public function __construct($width, $height)
    {
        $this->width = $width;
        $this->height = $height;
        $this->maze = array();
        $this->visited = array();

        // Initialize maze grid with walls
        for ($row = 0; $row < $height * 2 + 1; $row++) {
            for ($col = 0; $col < $width * 2 + 1; $col++) {
                $this->maze[$row][$col] = $col % 2 == 0 || $row % 2 == 0 ? '#' : ' ';
            }
        }
    }

    public function generateMaze()
    {
        $this->visitCell(1, 1);

        while (!empty($this->visited)) {
            $current = array_pop($this->visited);
            $neighbors = $this->getUnvisitedNeighbors($current[0], $current[1]);

            if (!empty($neighbors)) {
                $this->visited[] = $current;

                $randomNeighbor = $neighbors[rand(0, count($neighbors) - 1)];
                $this->removeWall($current[0], $current[1], $randomNeighbor[0], $randomNeighbor[1]);
                $this->visitCell($randomNeighbor[0], $randomNeighbor[1]);
            }
        }

        // Set entrance and exit
        $this->maze[1][0] = ' ';
        $this->maze[$this->height * 2 - 1][$this->width * 2] = ' ';
    }

    private function visitCell($row, $col)
    {
        $this->maze[$row][$col] = ' ';
        $this->visited[] = array($row, $col);
    }

    private function getUnvisitedNeighbors($row, $col)
    {
        $neighbors = array();

        if ($row - 2 > 0 && $this->maze[$row - 2][$col] == '#') {
            $neighbors[] = array($row - 2, $col);
        }
        if ($col + 2 < $this->width * 2 + 1 && $this->maze[$row][$col + 2] == '#') {
            $neighbors[] = array($row, $col + 2);
        }
        if ($row + 2 < $this->height * 2 + 1 && $this->maze[$row + 2][$col] == '#') {
            $neighbors[] = array($row + 2, $col);
        }
        if ($col - 2 > 0 && $this->maze[$row][$col - 2] == '#') {
            $neighbors[] = array($row, $col - 2);
        }

        return $neighbors;
    }

    private function removeWall($row1, $col1, $row2, $col2)
    {
        $this->maze[($row1 + $row2) / 2][($col1 + $col2) / 2] = ' ';
    }

    public function printMaze()
    {
        foreach ($this->maze as $row) {
            foreach ($row as $cell) {
                echo $cell;
            }
            echo PHP_EOL;
        }
    }
}

// Usage example
$width = 30;
$height = 15;

$mazeGenerator = new MazeGenerator($width, $height);
$mazeGenerator->generateMaze();
$mazeGenerator->printMaze();
```

This code creates a class called `MazeGenerator` that can generate random mazes of any given width and height using a depth-first search algorithm. The maze is represented using a 2D array, where walls are represented by '#' and empty paths are represented by spaces.

The `generateMaze` method initiates the maze generation process. It starts by initializing the maze grid with walls and then uses the depth-first search algorithm to carve paths through the maze. It keeps track of visited cells and randomly selects unvisited neighbors to connect them.

The `visitCell` method marks a cell as visited by changing its value to a space and adds it to the list of visited cells. The `getUnvisitedNeighbors` method returns a list of unvisited neighbors for a given cell. The `removeWall` method removes the wall between two cells, creating a path.

Finally, the `printMaze` method prints the generated maze to the console.

To use this code, you can adjust the variables `$width` and `$height` to your desired maze size. Then, create an instance of `MazeGenerator`, call the `generateMaze` method to generate the maze, and use `printMaze` to display the resulting maze on the console.