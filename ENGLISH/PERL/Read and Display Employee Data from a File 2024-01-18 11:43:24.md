```perl
#!/usr/bin/env perl

use strict;
use warnings;

# Declare a hash to store the employee data
my %employees;

# Open the employee data file
open(my $fh, '<', 'employees.txt') or die "Could not open the file: $!";

# Read each line of the file
while (my $line = <$fh>) {
  # Split the line into fields
  my @fields = split /,/, $line;

  # Store the employee data in the hash
  $employees{$fields[0]} = {
    name => $fields[1],
    department => $fields[2],
    salary => $fields[3],
  };
}

# Close the file
close($fh);

# Print the employee data
foreach my $id (sort keys %employees) {
  my $employee = $employees{$id};

  print "Employee ID: $id\n";
  print "Name: $employee->{name}\n";
  print "Department: $employee->{department}\n";
  print "Salary: $employee->{salary}\n\n";
}
```

This code reads employee data from a file called `employees.txt` and stores it in a hash. The data in the file is comma-separated, and each line contains the employee's ID, name, department, and salary.

The code uses the `open()` function to open the file for reading. The `while()` loop is used to read each line of the file, and the `split()` function is used to split the line into fields.

The `$employees` hash is used to store the employee data. The key for each employee is their ID, and the value is a reference to a hash containing the employee's name, department, and salary.

After all the employee data has been read from the file, the code uses the `foreach()` loop to print the data for each employee. The `sort()` function is used to sort the employee IDs before printing.

This code is complex because it uses several different Perl features, including hashes, references, and loops. However, it is also a very powerful piece of code, and it can be used to perform a variety of tasks.

Here is an example of how to use this code:

```perl
#!/usr/bin/env perl

use strict;
use warnings;

# Use the employee data from the previous example
my %employees = (
  1 => {
    name => 'John Doe',
    department => 'Sales',
    salary => '50000',
  },
  2 => {
    name => 'Jane Smith',
    department => 'Marketing',
    salary => '60000',
  },
);

# Get the name of the employee with ID 2
my $name = $employees{2}{name};

# Print the employee's name
print "The employee with ID 2 is $name.\n";
```

This code uses the `$employees` hash from the previous example to get the name of the employee with ID 2. The `print()` function is then used to print the employee's name.

You can use this code to perform a variety of tasks, such as finding the highest paid employee, calculating the total salary for all employees in a department, or printing a list of all employees who have a certain name.