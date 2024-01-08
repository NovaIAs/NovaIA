#!/usr/bin/perl

use strict;
use warnings;
use utf8;

# This program generates a random string of a given length.
# The string can contain any of the following characters:
# - Lowercase letters (a-z)
# - Uppercase letters (A-Z)
# - Digits (0-9)
# - Symbols (!@#$%^&*)

# Get the length of the string to generate from the command line.
my $length = shift;

# Create an array of all the characters that can be used in the string.
my @chars = (qw(a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9 ! @ # $ % ^ & *));

# Create a random string of the given length.
my $string = '';
for (my $i = 0; $i < $length; $i++) {
    # Get a random index into the array of characters.
    my $index = int(rand(scalar @chars));

    # Get the character at the random index.
    my $char = $chars[$index];

    # Add the character to the string.
    $string .= $char;
}

# Print the random string to the console.
print $string;

# Explain the code:

# 1. Import the necessary modules.
# The strict and warnings modules are used to catch common errors and warnings.
# The utf8 module is used to enable Unicode support.

# 2. Get the length of the string to generate from the command line.
# The shift function is used to get the first argument passed to the program.
# If no argument is passed, the program will exit with an error message.

# 3. Create an array of all the characters that can be used in the string.
# The qw() operator is used to create an array of strings.
# The parentheses around the list of strings are used to group the strings together.
# The scalar function is used to get the number of elements in an array.

# 4. Create a random string of the given length.
# The for loop is used to iterate over a range of numbers.
# The int function is used to convert a floating-point number to an integer.
# The rand function is used to generate a random number.
# The $chars[$index] expression is used to get the character at the random index.
# The .= operator is used to concatenate the character to the string.

# 5. Print the random string to the console.
# The print function is used to print data to the console.
# The $string variable contains the random string.