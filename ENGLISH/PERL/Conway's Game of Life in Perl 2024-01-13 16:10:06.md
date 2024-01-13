```perl
use strict;
use warnings;

# Define a hash to store the state of each cell
my %cells;

# Define the rules of the game
my %rules = (
    'live' => {
        0 => 'dead',
        1 => 'live',
        2 => 'live',
        3 => 'live',
        4 => 'dead',
        5 => 'dead',
        6 => 'dead',
        7 => 'dead',
        8 => 'dead',
    },
    'dead' => {
        0 => 'dead',
        1 => 'dead',
        2 => 'dead',
        3 => 'live',
        4 => 'dead',
        5 => 'dead',
        6 => 'dead',
        7 => 'dead',
        8 => 'dead',
    },
);

# Define the initial state of the cells
my @cells = (
    [0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 1, 1, 1, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0],
);

# Run the game for a specified number of generations
my $generations = 10;
for (my $i = 0; $i < $generations; $i++) {
    # Calculate the next state of each cell
    for (my $row = 0; $row < scalar @cells; $row++) {
        for (my $col = 0; $col < scalar @{$cells[$row]}; $col++) {
            # Count the number of live neighbors
            my $neighbors = 0;
            for (my $dr = -1; $dr <= 1; $dr++) {
                for (my $dc = -1; $dc <= 1; $dc++) {
                    next if $dr == 0 && $dc == 0; # Don't count the cell itself
                    my $r = $row + $dr;
                    my $c = $col + $dc;
                    if ($r >= 0 && $r < scalar @cells && $c >= 0 && $c < scalar @{$cells[$r]}) {
                        $neighbors++ if $cells[$r][$c] eq 'live';
                    }
                }
            }

            # Apply the rules to determine the next state of the cell
            $cells[$row][$col] = $rules{$cells[$row][$col]}{$neighbors};
        }
    }

    # Print the current state of the grid
    for (my $row = 0; $row < scalar @cells; $row++) {
        for (my $col = 0; $col < scalar @{$cells[$row]}; $col++) {
            print $cells[$row][$col], ' ';
        }
        print "\n";
    }

    # Wait for a key press before continuing
    print "Press any key to continue...";
    my $input = <STDIN>;
}

# Explanation

This Perl code simulates Conway's Game of Life, a cellular automaton that generates complex patterns from simple rules.

The %cells hash is used to store the state of each cell in the grid. The keys are the coordinates of the cell (row, column), and the values are the state of the cell ('live' or 'dead').

The %rules hash defines the rules of the game. The keys are the current state of the cell, and the values are a hash that maps the number of live neighbors to the next state of the cell.

The @cells array is the initial state of the grid.

The for loop runs the game for a specified number of generations.

Inside the loop, there is a nested loop that iterates over each cell in the grid.

For each cell, the code counts the number of live neighbors.

The code then applies the rules to determine the next state of the cell.

The code then prints the current state of the grid.

The code waits for a key press before continuing to the next generation.

The code repeats this process for the specified number of generations.