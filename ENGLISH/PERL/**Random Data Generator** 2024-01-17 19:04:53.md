```perl
use strict;
use warnings;

# Define a subroutine to generate a random number between two values
sub random_number {
  my ($min, $max) = @_;
  my $range = $max - $min;
  return int(rand($range)) + $min;
}

# Define a subroutine to generate a random string of a given length
sub random_string {
  my ($length) = @_;
  my $chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  my $string = "";
  for (my $i = 0; $i < $length; $i++) {
    my $index = random_number(0, length($chars) - 1);
    $string .= substr($chars, $index, 1);
  }
  return $string;
}

# Define a subroutine to generate a random array of a given length
sub random_array {
  my ($length) = @_;
  my @array = ();
  for (my $i = 0; $i < $length; $i++) {
    my $value = random_number(0, 100);
    push @array, $value;
  }
  return @array;
}

# Define a subroutine to generate a random hash of a given size
sub random_hash {
  my ($size) = @_;
  my %hash = ();
  for (my $i = 0; $i < $size; $i++) {
    my $key = random_string(10);
    my $value = random_number(0, 100);
    $hash{$key} = $value;
  }
  return %hash;
}

# Generate a random number between 1 and 10
my $random_number = random_number(1, 10);

# Generate a random string of length 10
my $random_string = random_string(10);

# Generate a random array of length 10
my @random_array = random_array(10);

# Generate a random hash of size 10
my %random_hash = random_hash(10);

# Print the results
print "Random number: $random_number\n";
print "Random string: $random_string\n";
print "Random array: @random_array\n";
print "Random hash: %random_hash\n";
```

Explanation:

The code defines four subroutines to generate random data: a random number, a random string, a random array, and a random hash. These subroutines are then used to generate random data of different types and sizes. The results are then printed to the console.

Here is a breakdown of the code:

* The `random_number` subroutine takes two arguments: the minimum and maximum values for the random number. It generates a random number between these two values and returns it.
* The `random_string` subroutine takes one argument: the length of the random string. It generates a string of random characters of the specified length and returns it.
* The `random_array` subroutine takes one argument: the length of the random array. It generates an array of random numbers of the specified length and returns it.
* The `random_hash` subroutine takes one argument: the size of the random hash. It generates a hash of random key-value pairs of the specified size and returns it.

The main part of the code uses these subroutines to generate random data of different types and sizes. It then prints the results to the console.

This code is complex and has a lot of different parts, but it is also very flexible and can be used to generate random data of any type or size.