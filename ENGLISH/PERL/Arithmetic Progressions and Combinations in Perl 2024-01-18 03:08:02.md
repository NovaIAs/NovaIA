```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my $number = shift;
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to calculate the permutations of a set of elements
sub permutations {
    my @elements = @_;
    my @permutations;
    for (my $i = 0; $i < scalar @elements; $i++) {
        my @remaining_elements = @elements;
        splice @remaining_elements, $i, 1;
        my @sub_permutations = permutations(@remaining_elements);
        for (my $j = 0; $j < scalar @sub_permutations; $j++) {
            @permutations = (@permutations, $elements[$i] . @sub_permutations[$j]);
        }
    }
    return @permutations;
}

# Define a subroutine to calculate the combinations of a set of elements
sub combinations {
    my @elements = @_;
    my @combinations;
    for (my $i = 0; $i < scalar @elements; $i++) {
        my @remaining_elements = @elements;
        splice @remaining_elements, $i, 1;
        my @sub_combinations = combinations(@remaining_elements);
        for (my $j = 0; $j < scalar @sub_combinations; $j++) {
            @combinations = (@combinations, $elements[$i] . @sub_combinations[$j]);
        }
    }
    return @combinations;
}

# Define a subroutine to calculate the power set of a set of elements
sub power_set {
    my @elements = @_;
    my @power_set;
    for (my $i = 0; $i < 2**scalar @elements; $i++) {
        my @subset;
        for (my $j = 0