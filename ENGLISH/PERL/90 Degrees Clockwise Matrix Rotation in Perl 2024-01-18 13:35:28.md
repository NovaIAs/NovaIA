```perl
use strict;
use warnings;
use feature 'say';

# Declare a constant for the number of rows and columns in the matrix
my $SIZE = 5;

# Create a 2D array to represent the matrix
my @matrix = (
    [1, 2, 3, 4, 5],
    [6, 7, 8, 9, 10],
    [11, 12, 13, 14, 15],
    [16, 17, 18, 19, 20],
    [21, 22, 23, 24, 25]
);

# Create a subroutine to print the matrix
sub print_matrix {
    my ($matrix) = @_;

    for my $row (0 .. $SIZE - 1) {
        for my $column (0 .. $SIZE - 1) {
            printf "%3d ", $matrix->[$row][$column];
        }
        say '';
    }
}

# Print the original matrix
say "Original matrix:";
print_matrix(\@matrix);

# Transpose the matrix
for my $row (0 .. $SIZE - 1) {
    for my $column (0 .. $row) {
        my $temp = $matrix[$row][$column];
        $matrix[$row][$column] = $matrix[$column][$row];
        $matrix[$column][$row] = $temp;
    }
}

# Reverse the elements in each row of the transposed matrix
for my $row (0 .. $SIZE - 1) {
    my $left = 0;
    my $right = $SIZE - 1;

    while ($left < $right) {
        my $temp = $matrix[$row][$left];
        $matrix[$row][$left] = $matrix[$row][$right];
        $matrix[$row][$right] = $temp;

        $left++;
        $right--;
    }
}

# Print the rotated matrix
say "Rotated matrix:";
print_matrix(\@matrix);
```

This Perl code performs a complex rotation on a 5x5 matrix. It starts by creating a 2D array to represent the original matrix. Then, it calls a subroutine to print the matrix.

The next step is to transpose the matrix. This is done by swapping the elements in each row and column. For example, the element in the first row and first column is swapped with the element in the first row and last column.

After transposing the matrix, the elements in each row are reversed. This is done by swapping the elements from the left and right ends of each row. For example, the first and last elements in the first row are swapped.

Finally, the rotated matrix is printed using the same subroutine as before.

Here is an example of the output of the code:

```
Original matrix:
  1  2  3  4  5
  6  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20
 21 22 23 24 25

Rotated matrix:
 21 16 11  6  1
 22 17 12  7  2
 23 18 13  8  3
 24 19 14  9  4
 25 20 15 10  5
```

As you can see, the matrix has been rotated 90 degrees clockwise.