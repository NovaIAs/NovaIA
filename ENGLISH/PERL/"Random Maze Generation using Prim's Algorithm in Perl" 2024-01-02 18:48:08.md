Certainly! Here's a complex and differentiated Perl code that generates a random maze using Prim's algorithm:

```perl
use strict;
use warnings;

# Maze size
my $width = 20;
my $height = 10;

# Initialize the maze grid
my @maze;
for (my $y = 0; $y < $height; $y++) {
    for (my $x = 0; $x < $width; $x++) {
        $maze[$y][$x] = {
            visited => 0,
            walls => [1, 1, 1, 1],  # top, right, bottom, left
        };
    }
}

# Starting point
my $start_x = int(rand($width));
my $start_y = int(rand($height));
$maze[$start_y][$start_x]{visited} = 1;

# Generate maze using Prim's algorithm
my @frontiers = ([$start_x, $start_y]);
while (@frontiers) {
    # Randomly select a frontier cell
    my $frontier_index = int(rand(scalar(@frontiers)));
    my ($frontier_x, $frontier_y) = @{$frontiers[$frontier_index]};
    splice(@frontiers, $frontier_index, 1);

    # Find unvisited neighbors
    my @neighbors;
    for my $dir ([0, -1], [1, 0], [0, 1], [-1, 0]) {
        my ($nx, $ny) = ($frontier_x + $dir->[0], $frontier_y + $dir->[1]);
        next if ($nx < 0 || $ny < 0 || $nx >= $width || $ny >= $height);
        push(@neighbors, [$nx, $ny]) unless ($maze[$ny][$nx]{visited});
    }

    if (@neighbors) {
        # Randomly select a neighbor
        my $neighbor_index = int(rand(scalar(@neighbors)));
        my ($neighbor_x, $neighbor_y) = @{$neighbors[$neighbor_index]};

        # Mark the neighbor as visited
        $maze[$neighbor_y][$neighbor_x]{visited} = 1;

        # Remove the wall between the frontier and the neighbor
        if ($neighbor_x - $frontier_x == 1) {
            $maze[$frontier_y][$frontier_x]{walls}[1] = 0;  # Remove right wall
            $maze[$neighbor_y][$neighbor_x]{walls}[3] = 0;  # Remove left wall
        } elsif ($neighbor_x - $frontier_x == -1) {
            $maze[$frontier_y][$frontier_x]{walls}[3] = 0;  # Remove left wall
            $maze[$neighbor_y][$neighbor_x]{walls}[1] = 0;  # Remove right wall
        } elsif ($neighbor_y - $frontier_y == 1) {
            $maze[$frontier_y][$frontier_x]{walls}[2] = 0;  # Remove bottom wall
            $maze[$neighbor_y][$neighbor_x]{walls}[0] = 0;  # Remove top wall
        } else {
            $maze[$frontier_y][$frontier_x]{walls}[0] = 0;  # Remove top wall
            $maze[$neighbor_y][$neighbor_x]{walls}[2] = 0;  # Remove bottom wall
        }

        # Add the neighbor to the frontiers list
        push(@frontiers, [$neighbor_x, $neighbor_y]);

        # Add more random walls to the maze
        for (my $i = 0; $i < 3; $i++) {
            my $random_x = int(rand($width));
            my $random_y = int(rand($height));
            $maze[$random_y][$random_x]{walls}[int(rand(4))] = 1;
        }
    }
}

# Print the maze
for (my $y = 0; $y < $height; $y++) {
    for (my $x = 0; $x < $width; $x++) {
        my $cell = $maze[$y][$x];
        my $walls = $cell->{walls};
        my $top = $walls->[0] ? "###" : "# #";
        my $left = $walls->[3] ? "#" : " ";
        my $right = $walls->[1] ? "#" : " ";
        my $bottom = $walls->[2] ? "###" : "# #";

        print "$top\n" if ($y == 0);
        print "$left $right\n";
        print "$bottom\n" if ($y == $height - 1);
    }
}
```

This code generates a random maze using Prim's algorithm. It starts by initializing a grid of cells, where each cell has four walls. It then randomly selects a starting point and marks it as visited.

The algorithm proceeds by selecting a random frontier cell from the list of frontiers. A frontier cell is a cell that has been visited but has unvisited neighbors. It randomly selects a neighbor cell, marks it as visited, and removes the wall between the frontier and the neighbor.

To add more complexity and variety to the maze, the algorithm also randomly adds additional walls to the maze during the generation process.

Finally, the code prints the maze by iterating over each cell and printing the appropriate characters to represent the walls and openings of the maze.

The resulting maze will be a 20x10 grid, visually represented with "#" characters for walls and spaces for openings.