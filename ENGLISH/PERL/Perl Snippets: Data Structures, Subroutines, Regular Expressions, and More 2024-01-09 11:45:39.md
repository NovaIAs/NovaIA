```perl
use strict;
use warnings;
use Data::Dumper;

# Define a complex data structure
my $data = {
    name => 'John Doe',
    age => 30,
    address => {
        street => '123 Main Street',
        city => 'Anytown',
        state => 'CA',
        zip => '12345'
    },
    hobbies => [
        'reading',
        'writing',
        'hiking'
    ]
};

# Print the data structure using Data::Dumper
print Data::Dumper->Dump($data, [qw/sort/]);

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my $n = shift;
    my $result = 1;
    for (my $i = 1; $i <= $n; $i++) {
        $result *= $i;
    }
    return $result;
}

# Print the factorial of 10
print "The factorial of 10 is ", factorial(10), "\n";

# Define a regular expression to match email addresses
my $email_regex = qr/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;

# Check if a given string is a valid email address
my $email = 'john.doe@example.com';
if ($email =~ $email_regex) {
    print "$email is a valid email address\n";
} else {
    print "$email is not a valid email address\n";
}

# Define a hash to store the number of occurrences of each letter in a string
my %letter_counts;

# Count the occurrences of each letter in a given string
my $string = 'Hello, world!';
for (my $i = 0; $i < length($string); $i++) {
    my $char = substr($string, $i, 1);
    $letter_counts{$char}++;
}

# Print the letter counts
print "Letter counts:\n";
foreach my $char (keys %letter_counts) {
    print "$char: $letter_counts{$char}\n";
}

# Define a subroutine to sort a list of numbers in descending order
sub sort_descending {
    my @numbers = @_;
    @numbers = sort { $b <=> $a } @numbers;
    return @numbers;
}

# Sort a list of numbers in descending order
my @numbers = (1, 3, 5, 2, 4);
@numbers = sort_descending(@numbers);
print "Sorted numbers in descending order: @numbers\n";

# Define a subroutine to calculate the Fibonacci sequence up to a given number
sub fibonacci {
    my $n = shift;
    my @fib_sequence = (0, 1);
    while (@fib_sequence[-1] < $n) {
        my $next_number = $fib_sequence[-2] + $fib_sequence[-1];
        push @fib_sequence, $next_number;
    }
    return @fib_sequence;
}

# Calculate the Fibonacci sequence up to 100
my @fib_sequence = fibonacci(100);
print "Fibonacci sequence up to 100: @fib_sequence\n";
```

This code is a collection of different Perl snippets that demonstrate various features of the language. It includes:

* Using a complex data structure (hash) and printing it using Data::Dumper.
* Defining a subroutine to calculate the factorial of a number.
* Using a regular expression to match email addresses.
* Counting the occurrences of each letter in a string.
* Defining a subroutine to sort a list of numbers in descending order.
* Calculating the Fibonacci sequence up to a given number.