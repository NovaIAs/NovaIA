```perl
use strict;
use warnings;

my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'city' => 'New York',
    'country' => 'USA',
    'occupation' => 'Software Engineer',
    'hobbies' => ['reading', 'writing', 'traveling'],
    'education' => [
        {
            'school' => 'Harvard University',
            'degree' => 'Bachelor of Science in Computer Science',
            'year' => 2010,
        },
        {
            'school' => 'Stanford University',
            'degree' => 'Master of Science in Computer Science',
            'year' => 2012,
        },
    ],
    'work_experience' => [
        {
            'company' => 'Google',
            'position' => 'Software Engineer',
            'years' => 5,
        },
        {
            'company' => 'Facebook',
            'position' => 'Software Engineer',
            'years' => 3,
        },
    ],
);

# Print the data in a structured format
print "Name: $data{name}\n";
print "Age: $data{age}\n";
print "City: $data{city}\n";
print "Country: $data{country}\n";
print "Occupation: $data{occupation}\n";
print "Hobbies: " . join(", ", @{$data{hobbies}}) . "\n";

# Print the education information
print "Education:\n";
foreach my $education (@{$data{education}}) {
    print "\tSchool: $education->{school}\n";
    print "\tDegree: $education->{degree}\n";
    print "\tYear: $education->{year}\n";
}

# Print the work experience information
print "Work Experience:\n";
foreach my $work_experience (@{$data{work_experience}}) {
    print "\tCompany: $work_experience->{company}\n";
    print "\tPosition: $work_experience->{position}\n";
    print "\tYears: $work_experience->{years}\n";
}
```

This code creates a complex data structure in Perl, using a hash (%data) to store information about a person. The data includes the person's name, age, city, country, occupation, hobbies, education, and work experience.

The code then prints the data in a structured format, using loops to iterate over the arrays of hobbies, education, and work experience.

This code is complex because it uses a variety of data structures and control flow statements. It also demonstrates the use of loops to iterate over arrays and hashes.