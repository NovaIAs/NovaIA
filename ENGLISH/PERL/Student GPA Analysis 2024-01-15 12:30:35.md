```
use strict;
use warnings;

# Define a hash to store the information about each student
my %students = (
    'John Doe' => {
        'age' => 20,
        'gender' => 'male',
        'gpa' => 3.5,
    },
    'Jane Smith' => {
        'age' => 21,
        'gender' => 'female',
        'gpa' => 3.8,
    },
    'Michael Jones' => {
        'age' => 22,
        'gender' => 'male',
        'gpa' => 3.2,
    },
);

# Define a subroutine to calculate the average GPA of all students
sub calculate_average_gpa {
    my $total_gpa = 0;
    my $num_students = 0;

    # Iterate over each student in the hash
    foreach my $student_name (keys %students) {
        # Get the student's GPA
        my $student_gpa = $students{$student_name}{'gpa'};

        # Add the student's GPA to the total
        $total_gpa += $student_gpa;

        # Increment the number of students
        $num_students++;
    }

    # Calculate the average GPA
    my $average_gpa = $total_gpa / $num_students;

    # Return the average GPA
    return $average_gpa;
}

# Define a subroutine to find the student with the highest GPA
sub find_student_with_highest_gpa {
    my $highest_gpa = 0;
    my $student_with_highest_gpa;

    # Iterate over each student in the hash
    foreach my $student_name (keys %students) {
        # Get the student's GPA
        my $student_gpa = $students{$student_name}{'gpa'};

        # If the student's GPA is higher than the current highest GPA, update the highest GPA and the student with the highest GPA
        if ($student_gpa > $highest_gpa) {
            $highest_gpa = $student_gpa;
            $student_with_highest_gpa = $student_name;
        }
    }

    # Return the student with the highest GPA
    return $student_with_highest_gpa;
}

# Define a subroutine to find the student with the lowest GPA
sub find_student_with_lowest_gpa {
    my $lowest_gpa = 4.0;
    my $student_with_lowest_gpa;

    # Iterate over each student in the hash
    foreach my $student_name (keys %students) {
        # Get the student's GPA
        my $student_gpa = $students{$student_name}{'gpa'};

        # If the student's GPA is lower than the current lowest GPA, update the lowest GPA and the student with the lowest GPA
        if ($student_gpa < $lowest_gpa) {
            $lowest_gpa = $student_gpa;
            $student_with_lowest_gpa = $student_name;
        }
    }

    # Return the student with the lowest GPA
    return $student_with_lowest_gpa;
}

# Calculate the average GPA of all students
my $average_gpa = calculate_average_gpa();

# Find the student with the highest GPA
my $student_with_highest_gpa = find_student_with_highest_gpa();

# Find the student with the lowest GPA
my $student_with_lowest_gpa = find_student_with_lowest_gpa();

# Print the results
print "Average GPA: $average_gpa\n";
print "Student with highest GPA: $student_with_highest_gpa\n";
print "Student with lowest GPA: $student_with_lowest_gpa\n";
```

Explanation:

The code you provided is a Perl script that performs various operations on a hash of student information. The hash contains information about each student, including their name, age, gender, and GPA.

The code first defines a subroutine called `calculate_average_gpa()` that calculates the average GPA of all students. The subroutine iterates over each student in the hash, adds their GPA to a total, and then divides the total by the number of students to get the average GPA.

The code then defines a subroutine called `find_student_with_highest_gpa()` that finds the student with the highest GPA. The subroutine iterates over each student in the hash, and if the student's GPA is higher than the current highest GPA, the subroutine updates the highest GPA and the student with the highest GPA.

The code then defines a subroutine called `find_student_with_lowest_gpa()` that finds the student with the lowest GPA. The subroutine iterates over each student in the hash, and if the student's GPA is lower than the current lowest GPA, the subroutine updates the lowest GPA and the student with the lowest GPA.

Finally, the code calls the three subroutines to calculate the average GPA of all students, find the student with the highest GPA, and find the student with the lowest GPA. The code then prints the results to the console.