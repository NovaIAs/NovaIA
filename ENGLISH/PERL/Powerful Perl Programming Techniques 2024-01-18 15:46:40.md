```perl
# Demonstrates complex Perl programming techniques

use strict;
use warnings;

# Define a custom type for a person
package Person {
    sub new {
        my ($class, %args) = @_;
        bless {
            name    => $args{name},
            age     => $args{age},
            gender  => $args{gender},
        }, $class;
    }

    sub greet {
        my ($self) = @_;
        return "Hello, my name is {$self->{name}}.";
    }
}

# Create a hash reference to store people
my $people = {
    alice => Person->new(name => 'Alice', age => 25, gender => 'female'),
    bob   => Person->new(name => 'Bob',   age => 30, gender => 'male'),
    charlie => Person->new(name => 'Charlie', age => 35, gender => 'male'),
};

# Iterate over the people hash reference and print each person's greeting
foreach my $name (keys %{$people}) {
    print $people->{$name}->greet . "\n";
}

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($number) = @_;
    return 1 if $number == 0;
    return $number * factorial($number - 1);
}

# Print the factorial of 5
print factorial(5) . "\n";

# Define a subroutine to flatten a multidimensional array
sub flatten {
    my (@array) = @_;
    my @flattened;
    foreach my $element (@array) {
        if (ref $element eq 'ARRAY') {
            push @flattened, flatten(@$element);
        } else {
            push @flattened, $element;
        }
    }
    return @flattened;
}

# Flatten a multidimensional array and print the result
my @multidimensional = (1, 2, 3, [4, 5, 6], 7, 8, 9, [10, 11, 12]);
print join(', ', flatten(@multidimensional)) . "\n";

# Define a subroutine to find the longest common substring between two strings
sub longest_common_substring {
    my ($string1, $string2) = @_;
    my $lcs = '';
    for (my $i = 0; $i < length($string1); $i++) {
        for (my $j = $i + 1; $j <= length($string1); $j++) {
            my $substring = substr($string1, $i, $j - $i);
            if ($string2 =~ /$substring/ and length($substring) > length($lcs)) {
                $lcs = $substring;
            }
        }
    }
    return $lcs;
}

# Find the longest common substring between two strings and print the result
my $string1 = 'ABCDFGHIJKLMNOPQRSTUVWXYZ';
my $string2 = 'CDEFGHIJKMNOPQRSTUVXYZ';
print longest_common_substring($string1, $string2) . "\n";

# Define a subroutine to generate a random password
sub generate_password {
    my ($length) = @_;
    my $password = '';
    for (my $i = 0; $i < $length; $i++) {
        my $random_number = int(rand(95)) + 32;
        my $character = chr($random_number);
        $password .= $character;
    }
    return $password;
}

# Generate a random password and print the result
print generate_password(10) . "\n";
```

Explanation:

1. The code defines a custom type called `Person` using a package. It includes methods for creating a new person and greeting them.

2. A hash reference `$people` is created to store instances of the `Person` type.

3. The code iterates over the `$people` hash reference and prints the greeting for each person.

4. The `factorial` subroutine calculates the factorial of a given number using recursion.

5. The `flatten` subroutine flattens a multidimensional array into a one-dimensional array.

6. The `longest_common_substring` subroutine finds the longest common substring between two strings.

7. The `generate_password` subroutine generates a random password of a specified length.

This code demonstrates various Perl features such as custom types, subroutines, recursion, and working with data structures.