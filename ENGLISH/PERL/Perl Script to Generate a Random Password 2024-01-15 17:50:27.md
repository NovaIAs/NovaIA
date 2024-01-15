```perl
use strict;
use warnings;

# This is a Perl script that generates a random password of a given length.
# The password can contain uppercase and lowercase letters, numbers, and symbols.

# Get the desired password length from the user.
print "Enter the desired password length: ";
my $length = <STDIN>;
chomp $length;

# Create an array of all possible characters that can be used in the password.
my @chars = ('a'..'z', 'A'..'Z', '0'..'9', '!@#$%^&*()_+');

# Create a random password of the specified length.
my $password = '';
for (1..$length) {
    # Get a random index into the @chars array.
    my $index = int(rand(@chars));

    # Get the character at the random index.
    my $char = $chars[$index];

    # Add the character to the password.
    $password .= $char;
}

# Print the password to the console.
print "Your password is: $password\n";
```

This script first gets the desired password length from the user. It then creates an array of all possible characters that can be used in the password. The array contains uppercase and lowercase letters, numbers, and symbols.

Next, the script creates a random password of the specified length. It does this by looping through the desired password length and randomly selecting a character from the array of possible characters. The character is then added to the password.

Finally, the script prints the password to the console.