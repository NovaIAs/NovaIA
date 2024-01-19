```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number.
sub factorial {
    my $number = shift;
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to generate a random number between two numbers.
sub random_number {
    my $min = shift;
    my $max = shift;
    my $range = $max - $min + 1;
    my $random_number = int(rand($range)) + $min;
    return $random_number;
}

# Define a subroutine to sort an array of numbers.
sub sort_numbers {
    my @array = @_;
    @array = sort { $a <=> $b } @array;
    return @array;
}

# Define a subroutine to find the median of an array of numbers.
sub median {
    my @array = @_;
    @array = sort_numbers(@array);
    my $length = scalar(@array);
    if ($length % 2 == 0) {
        my $middle_index = $length / 2;
        my $median = ($array[$middle_index - 1] + $array[$middle_index]) / 2;
        return $median;
    } else {
        my $middle_index = ($length + 1) / 2;
        my $median = $array[$middle_index - 1];
        return $median;
    }
}

# Define a subroutine to find the mode of an array of numbers.
sub mode {
    my @array = @_;
    my %hash;
    foreach my $number (@array) {
        $hash{$number}++;
    }
    my @modes;
    my $max_count = 0;
    foreach my $number (keys %hash) {
        if ($hash{$number} > $max_count) {
            @modes = ();
            $max_count = $hash{$number};
        }
        if ($hash{$number} == $max_count) {
            push @modes, $number;
        }
    }
    return @modes;
}

# Define a subroutine to find the standard deviation of an array of numbers.
sub standard_deviation {
    my @array = @_;
    my $mean = mean(@array);
    my $sum_of_squared_differences = 0;
    foreach my $number (@array) {
        $sum_of_squared_differences += ($number - $mean) ** 2;
    }
    my $variance = $sum_of_squared_differences / (scalar(@array) - 1);
    my $standard_deviation = sqrt($variance);
    return $standard_deviation;
}

# Define a subroutine to find the mean of an array of numbers.
sub mean {
    my @array = @_;
    my $sum = 0;
    foreach my $number (@array) {
        $sum += $number;
    }
    my $mean = $sum / scalar(@array);
    return $mean;
}

# Define a subroutine to find the range of an array of numbers.
sub range {
    my @array = @_;
    my $min = $array[0];
    my $max = $array[0];
    foreach my $number (@array) {
        if ($number < $min) {
            $min = $number;
        }
        if ($number > $max) {
            $max = $number;
        }
    }
    my $range = $max - $min;
    return $range;
}

# Define a subroutine to find the variance of an array of numbers.
sub variance {
    my @array = @_;
    my $mean = mean(@array);
    my $sum_of_squared_differences = 0;
    foreach my $number (@array) {
        $sum_of_squared_differences += ($number - $mean) ** 2;
    }
    my $variance = $sum_of_squared_differences / (scalar(@array) - 1);
    return $variance;
}

# Define a subroutine to find the covariance of two arrays of numbers.
sub covariance {
    my @array1 = @_;
    my @array2 = @_;
    my $mean1 = mean(@array1);
    my $mean2 = mean(@array2);
    my $sum_of_products = 0;
    for (my $i = 0; $i < scalar(@array1); $i++) {
        $sum