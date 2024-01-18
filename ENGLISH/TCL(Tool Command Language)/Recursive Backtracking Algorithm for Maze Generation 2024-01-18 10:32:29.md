```tcl
proc generate_maze {width height} {
    # Create a 2D array to represent the maze
    set maze [lrepeat $height [lrepeat $width 0]]

    # Set the starting and ending points
    set start [list 0 [expr {$height - 1} / 2]]
    set end [list [expr {$width - 1}] [expr {$height - 1} / 2]]

    # Create a stack to store the cells to be processed
    set stack [list $start]

    # While there are still cells to be processed
    while {[llength $stack] > 0} {
        # Pop the top cell from the stack
        set cell [lpop $stack]

        # Mark the cell as visited
        set maze[$cell(1)][$cell(0)] 1

        # Get the neighbors of the cell
        set neighbors [get_neighbors $maze $cell]

        # Shuffle the neighbors to randomize the order
        shuffle $neighbors

        # For each neighbor
        foreach neighbor $neighbors {
            # If the neighbor is unvisited
            if {$maze[$neighbor(1)][$neighbor(0)] == 0} {
                # Add the neighbor to the stack
                lpush $stack $neighbor
            }
        }
    }

    # Return the maze
    return $maze
}

proc get_neighbors {maze cell} {
    # Get the coordinates of the cell
    set x $cell(0)
    set y $cell(1)

    # Create a list to store the neighbors
    set neighbors {}

    # If the cell is not on the left edge
    if {$x > 0} {
        # Add the cell to the left to the neighbors
        lappend neighbors [list [expr {$x - 1}] $y]
    }

    # If the cell is not on the right edge
    if {$x < [llength $maze] - 1} {
        # Add the cell to the right to the neighbors
        lappend neighbors [list [expr {$x + 1}] $y]
    }

    # If the cell is not on the top edge
    if {$y > 0} {
        # Add the cell above to the neighbors
        lappend neighbors [list $x [expr {$y - 1}]]
    }

    # If the cell is not on the bottom edge
    if {$y < [llength $maze[0]] - 1} {
        # Add the cell below to the neighbors
        lappend neighbors [list $x [expr {$y + 1}]]
    }

    # Return the neighbors
    return $neighbors
}

proc shuffle {list} {
    # Create a copy of the list
    set shuffled [lcopy $list]

    # For each element in the list
    for {set i 0} {$i < [llength $shuffled]} {incr i} {
        # Get a random index
        set j [rand [llength $shuffled]]

        # Swap the elements at the two indices
        set temp [lindex $shuffled $i]
        set shuffled [lset shuffled $i [lindex $shuffled $j]]
        set shuffled [lset shuffled $j $temp]
    }

    # Return the shuffled list
    return $shuffled
}

proc print_maze {maze} {
    # For each row in the maze
    foreach row $maze {
        # For each column in the row
        foreach col $row {
            # Print the cell
            if {$col == 1} {
                puts -n "*"
            } else {
                puts -n " "
            }
        }
        # Print a newline
        puts ""
    }
}

# Generate a maze
set maze [generate_maze 20 20]

# Print the maze
print_maze $maze
```

This code generates a random maze using the recursive backtracking algorithm. It creates a 2D array to represent the maze, with 1s representing walls and 0s representing open spaces. The algorithm starts at the top-left corner of the maze and randomly chooses a direction to move in. If the chosen direction is open, the algorithm moves in that direction and adds the previous cell to a stack. If the chosen direction is blocked, the algorithm pops the previous cell from the stack and tries a different direction. The algorithm continues until all cells in the maze have been visited.

The `generate_maze` procedure takes two arguments: the width and height of the maze. It returns a 2D array representing the maze.

The `get_neighbors` procedure takes two arguments: the maze and the current cell. It returns a list of the neighbors of the current cell.

The `shuffle` procedure takes a list as an argument and returns a shuffled copy of the list.

The `print_maze` procedure takes the maze as an argument and prints it to the console.