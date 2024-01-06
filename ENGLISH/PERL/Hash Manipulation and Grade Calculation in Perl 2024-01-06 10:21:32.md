```perl
use strict;
use warnings;

# Define a hash to store the names and ages of people.
my %people = (
    'John Doe' => 30,
    'Jane Smith' => 25,
    'Bill Jones' => 40,
    'Mary Johnson' => 35,
);

# Create a subroutine to print the names and ages of people in the hash.
sub print_people {
    my $hash_ref = shift;

    foreach my $name (sort keys %{$hash_ref}) {
        my $age = $hash_ref->{$name};
        print "$name is $age years old.\n";
    }
}

# Print the names and ages of people in the hash.
print_people(\%people);

# Define a hash to store the names and grades of students.
my %students = (
    'John Doe' => 90,
    'Jane Smith' => 85,
    'Bill Jones' => 75,
    'Mary Johnson' => 95,
);

# Create a subroutine to calculate the average grade of students in the hash.
sub calculate_average_grade {
    my $hash_ref = shift;

    my $total_grade = 0;
    my $num_students = 0;

    foreach my $name (keys %{$hash_ref}) {
        my $grade = $hash_ref->{$name};
        $total_grade += $grade;
        $num_students++;
    }

    my $average_grade = $total_grade / $num_students;

    return $average_grade;
}

# Calculate the average grade of students in the hash.
my $average_grade = calculate_average_grade(\%students);

# Print the average grade of students.
print "The average grade is $average_grade.\n";
```

This code defines two hashes, one to store the names and ages of people, and the other to store the names and grades of students. It then defines two subroutines, one to print the names and ages of people in a hash, and the other to calculate the average grade of students in a hash. The code then uses these subroutines to print the names and ages of people in the hash, and to calculate and print the average grade of students in the hash.

The code is complex and differentiated because it uses multiple data structures (hashes), subroutines, and control structures (loops and conditionals). It also demonstrates the use of references to hashes, which is a more advanced Perl concept.