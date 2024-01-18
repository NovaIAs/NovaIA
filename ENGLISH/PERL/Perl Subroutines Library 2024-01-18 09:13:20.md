```perl
use strict;
use warnings;

# Define a subroutine to greet a person
sub greet {
    my ($name) = @_;
    print "Hello, $name!\n";
}

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($n) = @_;
    if ($n == 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

# Define a subroutine to print a list of numbers
sub print_list {
    my @list = @_;
    foreach my $item (@list) {
        print "$item ";
    }
    print "\n";
}

# Define a subroutine to sort a list of numbers
sub sort_list {
    my @list = @_;
    @list = sort @list;
    return @list;
}

# Define a subroutine to find the maximum value in a list of numbers
sub max_value {
    my @list = @_;
    my $max = $list[0];
    foreach my $item (@list) {
        if ($item > $max) {
            $max = $item;
        }
    }
    return $max;
}

# Define a subroutine to find the minimum value in a list of numbers
sub min_value {
    my @list = @_;
    my $min = $list[0];
    foreach my $item (@list) {
        if ($item < $min) {
            $min = $item;
        }
    }
    return $min;
}

# Define a subroutine to calculate the average value in a list of numbers
sub average_value {
    my @list = @_;
    my $sum = 0;
    foreach my $item (@list) {
        $sum += $item;
    }
    return $sum / scalar(@list);
}

# Define a subroutine to find the mode value in a list of numbers
sub mode_value {
    my @list = @_;
    my %hash;
    foreach my $item (@list) {
        $hash{$item}++;
    }
    my @modes;
    my $max_count = 0;
    foreach my $item (keys %hash) {
        if ($hash{$item} > $max_count) {
            @modes = ($item);
            $max_count = $hash{$item};
        } elsif ($hash{$item} == $max_count) {
            push @modes, $item;
        }
    }
    return @modes;
}

# Define a subroutine to find the median value in a list of numbers
sub median_value {
    my @list = @_;
    @list = sort @list;
    my $middle = scalar(@list) / 2;
    if ($middle =~ /\./) {
        return ($list[$middle - 1]