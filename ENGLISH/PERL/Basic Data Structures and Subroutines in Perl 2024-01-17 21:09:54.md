```
#!/usr/bin/perl

use strict;
use warnings;

my $string = "This is a string";
my @array = (1, 2, 3, 4, 5);
my %hash = (
    'name' => 'John Doe',
    'age' => 30,
    'job' => 'Software Engineer'
);

sub print_string {
    my $str = shift;
    print "$str\n";
}

sub print_array {
    my @arr = @_;
    foreach my $element (@arr) {
        print "$element ";
    }
    print "\n";
}

sub print_hash {
    my %hsh = @_;
    foreach my $key (keys %hsh) {
        print "$key: $hsh{$key}\n";
    }
}

print_string $string;
print_array @array;
print_hash %hash;
```

This code is a Perl script that defines a string, an array, and a hash, and then defines three subroutines (functions) to print each of them. The subroutines are called and the output is printed to the console.

Here is a breakdown of the code:

* The first three lines are Perl shebang lines. They tell the operating system what interpreter to use to run the script. In this case, the script should be run using the Perl interpreter.
* The next three lines import the `strict` and `warnings` modules. These modules help to enforce good programming practices and warn you about potential problems in your code.
* The next six lines define the string, array, and hash variables.
* The next three lines define the `print_string`, `print_array`, and `print_hash` subroutines. Each subroutine takes one argument, which is the data structure to be printed. The subroutines use a `foreach` loop to iterate over the data structure and print each element.
* The last three lines call the `print_string`, `print_array`, and `print_hash` subroutines and print the output to the console.

When you run this script, you will see the following output:

```
This is a string
1 2 3 4 5
name: John Doe
age: 30
job: Software Engineer
```