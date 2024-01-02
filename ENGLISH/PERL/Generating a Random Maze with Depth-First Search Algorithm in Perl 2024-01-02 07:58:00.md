Certainly! Here's a complex Perl code that generates a random maze using a depth-first search algorithm. The code is well-documented to explain each step:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Constants for maze dimensions
my $ROWS = 20;
my $COLS = 40;

# Initialize maze grid
my @maze;
for (my $row = 0; $row < $ROWS; $row++) {
    for (my $col = 0; $col < $COLS; $col++) {
        $maze[$row][$col] = '#';
    }
}

# Depth-first search algorithm to generate the maze
sub generate_maze {
    my ($row, $col) = @_;
    $maze[$row][$col] = ' ';  # Mark current cell as empty

    # Define possible movements: up, right, down, left
    my @movements = (
        [-1, 0],  # Up
        [0, 1],   # Right
        [1, 0],   # Down
        [0, -1]   # Left
    );

    # Randomize the order of movements
    @movements = map { $_->[0] }
        sort { $a->[1] <=> $b->[1] }
        map { [$_, rand()] } @movements;

    # Visit each neighbor cell
    for my $movement (@movements) {
        my $next_row = $row + $movement->[0] * 2;
        my $next_col = $col + $movement->[1] * 2;

        next if $next_row < 0 || $next_row >= $ROWS || $next_col < 0 || $next_col >= $COLS;
        next if $maze[$next_row][$next_col] ne '#';

        $maze[$row + $movement->[0]][$col + $movement->[1]] = ' ';  # Mark the cell as empty
        generate_maze($next_row, $next_col);  # Recursively call generate_maze
    }
}

# Start generating the maze from the top-left cell
generate_maze(0, 0);

# Print the generated maze
for (my $row = 0; $row < $ROWS; $row++) {
    for (my $col = 0; $col < $COLS; $col++) {
        print $maze[$row][$col];
    }
    print "\n";
}
```

Explanation:

1. The code starts by defining the dimensions of the maze (20 rows and 40 columns) and initializing the maze grid as a two-dimensional array filled with walls ('#').

2. The `generate_maze` subroutine uses the depth-first search algorithm to create the maze. It takes the current row and column as input and marks the current cell as empty (' ').

3. The algorithm defines the possible movements (up, right, down, left) and randomizes their order.

4. For each movement, the algorithm calculates the coordinates of the next cell to visit by moving two steps in the given direction. If the next cell is out of bounds or has already been visited, the algorithm skips to the next movement.

5. If the next cell is valid, it marks the intermediate cell (between the current and next cell) as empty and recursively calls the `generate_maze` subroutine with the next cell coordinates.

6. The maze generation starts from the top-left cell (0, 0) and recursively explores all possible paths until there are no unvisited cells left.

7. Finally, the generated maze is printed by iterating over the maze grid and printing each cell's content. Walls are represented by '#' and empty cells by ' '.

This code will generate a unique maze every time you run it. The size of the maze can be adjusted by changing the values of `$ROWS` and `$COLS`. Feel free to experiment with different maze sizes and explore further modifications to the code!