```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my $number = $_[0];
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to generate a random number between two values
sub random_number {
    my $min = $_[0];
    my $max = $_[1];
    return int(rand() * ($max - $min + 1)) + $min;
}

# Define a subroutine to shuffle an array
sub shuffle {
    my @array = @_;
    for (my $i = 0; $i < scalar(@array); $i++) {
        my $j = random_number(0, scalar(@array) - 1);
        my $temp = $array[$i];
        $array[$i] = $array[$j];
        $array[$j] = $temp;
    }
    return @array;
}

# Define a subroutine to find the maximum value in an array
sub max {
    my @array = @_;
    my $max_value = $array[0];
    for (my $i = 1; $i < scalar(@array); $i++) {
        if ($array[$i] > $max_value) {
            $max_value = $array[$i];
        }
    }
    return $max_value;
}

# Define a subroutine to find the minimum value in an array
sub min {
    my @array = @_;
    my $min_value = $array[0];
    for (my $i = 1; $i < scalar(@array); $i++) {
        if ($array[$i] < $min_value) {
            $min_value = $array[$i];
        }
    }
    return $min_value;
}

# Define a subroutine to calculate the average of an array
sub average {
    my @array = @_;
    my $sum = 0;
    foreach my $value (@array) {
        $sum += $value;
    }
    return $sum / scalar(@array);
}

# Define a subroutine to calculate the median of an array
sub median {
    my @array = @_;
    @array = sort @array;
    my $middle = int(scalar(@array) / 2);
    if (scalar(@array) % 2 == 0) {
        return ($array[$middle] + $array[$middle - 1]) / 2;
    } else {
        return $array[$middle];
    }
}

# Define a subroutine to calculate the mode of an array
sub mode {
    my @array = @_;
    my %counts;
    foreach my $value (@array) {
        $counts{$value}++;
    }
    my @