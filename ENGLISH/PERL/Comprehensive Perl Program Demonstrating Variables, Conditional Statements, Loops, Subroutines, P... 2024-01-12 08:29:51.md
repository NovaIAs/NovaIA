```perl
use strict;
use warnings;

# Declare variables
my $number = 10;
my $string = "Hello, world!";
my @array = (1, 2, 3, 4, 5);
my %hash = (
    name => "John Doe",
    age => 30,
    city => "New York",
);

# Conditional statements

# If statement
if ($number > 5) {
    print "The number is greater than 5.\n";
} elsif ($number == 5) {
    print "The number is equal to 5.\n";
} else {
    print "The number is less than 5.\n";
}

# For loop
for my $i (0 .. $#array) {
    print "Element $i of the array is $array[$i].\n";
}

# While loop
my $count = 0;
while ($count < 10) {
    print "The count is $count.\n";
    $count++;
}

# Do while loop
do {
    print "The count is $count.\n";
    $count++;
} while ($count < 10);

# Until loop
until ($count == 10) {
    print "The count is $count.\n";
    $count++;
}

# Foreach loop
foreach my $key (keys %hash) {
    print "The key is $key and the value is $hash{$key}.\n";
}

# Subroutines

sub add_numbers {
    my ($num1, $num2) = @_;
    return $num1 + $num2;
}

my $sum = add_numbers(3, 5);
print "The sum of 3 and 5 is $sum.\n";

# Packages

package MyPackage;

sub new {
    my ($class, %args) = @_;
    bless {
        name => $args{name},
        age => $args{age},
    }, $class;
}

sub get_name {
    my ($self) = @_;
    return $self->{name};
}

sub get_age {
    my ($self) = @_;
    return $self->{age};
}

package main;

my $object = MyPackage->new(
    name => "John Doe",
    age => 30,
);

print "The object's name is ", $object->get_name, " and his age is ", $object->get_age, ".\n";

# Regular expressions

# Match a word that starts with "a" and ends with "e"
my $pattern = qr/^a.*e$/;

# Search for the pattern in the string
my $result = $string =~ $pattern;

# Print the result
print "The result is $result.\n";

# File handling

open(my $file, "<", "test.txt") or die "Could not open file: $!";

while (my $line = <$file>) {
    print "Line: $line";
}

close($file);

# Exception handling

eval {
    # Code that may throw an exception
    die "An error occurred!";
};

if ($@) {
    # Handle the exception
    print "An error occurred: $@\n";
}

# Modules

use Math::Trig;

my $angle = 45;

# Calculate the sine of the angle
my $sine = sin($angle);

# Print the result
print "The sine of $angle is $sine.\n";

# Object-oriented programming

package Animal;

sub new {
    my ($class, %args) = @_;
    bless {
        name => $args{name},
        age => $args{age},
    }, $class;
}

sub get_name {
    my ($self) = @_;
    return $self->{name};
}

sub get_age {
    my ($self) = @_;
    return $self->{age};
}

sub speak {
    my ($self) = @_;
    return "I am an animal.";
}

package Dog;

@ISA = qw(Animal);

sub new {
    my ($class, %args) = @_;
    bless {
        name => $args{name},
        age => $args{age},
        breed => $args{breed},
    }, $class;
}

sub get_breed {
    my ($self) = @_;
    return $self->{breed};
}

sub speak {
    my ($self) = @_;
    return "Woof!";
}

package Cat;

@ISA = qw(Animal);

sub new {
    my ($class, %args) = @_;
    bless {
        name => $args{name},
        age => $args{age},
        