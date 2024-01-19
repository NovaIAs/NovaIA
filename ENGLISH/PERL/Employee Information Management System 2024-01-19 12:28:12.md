```perl
use strict;
use warnings;
use Data::Dumper;

# A complex data structure to store the employee information
my %employees = (
    'John Doe' => {
        'id'         => 1,
        'department' => 'Engineering',
        'salary'     => 100000,
        'benefits'   => {
            'health_insurance' => 'Platinum',
            'dental_insurance' => 'Gold',
            'vision_insurance' => 'Silver'
        }
    },
    'Jane Smith' => {
        'id'         => 2,
        'department' => 'Sales',
        'salary'     => 80000,
        'benefits'   => {
            'health_insurance' => 'Gold',
            'dental_insurance' => 'Silver',
            'vision_insurance' => 'Bronze'
        }
    },
    'Michael Jones' => {
        'id'         => 3,
        'department' => 'Marketing',
        'salary'     => 70000,
        'benefits'   => {
            'health_insurance' => 'Silver',
            'dental_insurance' => 'Bronze',
            'vision_insurance' => 'None'
        }
    }
);

# Print the employee information using a subroutine
sub print_employee_info {
    my $employee_name = shift;
    my $employee_data = $employees{$employee_name};

    print "Employee Name: $employee_name\n";
    print "Employee ID: $employee_data->{id}\n";
    print "Department: $employee_data->{department}\n";
    print "Salary: $employee_data->{salary}\n";
    print "Benefits:\n";

    foreach my $benefit (keys %{$employee_data->{benefits}}) {
        print "  $benefit: $employee_data->{benefits}{$benefit}\n";
    }

    print "\n";
}

# Print the employee information for each employee
foreach my $employee_name (keys %employees) {
    print_employee_info($employee_name);
}

# Calculate the total salary of all employees
my $total_salary = 0;
foreach my $employee_data (values %employees) {
    $total_salary += $employee_data->{salary};
}

print "Total Salary: $total_salary\n";

# Find the employee with the highest salary
my $highest_salary = 0;
my $highest_salary_employee;
foreach my $employee_name (keys %employees) {
    my $employee_data = $employees{$employee_name};
    if ($employee_data->{salary} > $highest_salary) {
        $highest_salary = $employee_data->{salary};
        $highest_salary_employee = $employee_name;
    }
}

print "Highest Salary: $highest_salary\n";
print "Highest Salary Employee: $highest_salary_employee\n";

# Group the employees by department
my %employees_by_department;
foreach my $employee_name (keys %employees) {
    my $employee_data = $employees{$employee_name};
    push @{$employees_by_department{$employee_data->{department}}}, $employee_name;
}

print "Employees by Department:\n";
foreach my $department (keys %employees_by_department) {
    print "  $department:\n";
    foreach my $employee_name (@{$employees_by_department{$department}}) {
        print "    $employee_name\n";
    }
}
```

Explanation:

1. We start by creating a complex data structure (%employees) to store the employee information. This data structure is a hash of hashes, where the keys are the employee names and the values are hashes containing the employee's id, department, salary, and benefits.


2. We define a subroutine print_employee_info() to print the employee information in a structured format. This subroutine takes an employee's name as an argument and prints their id, department, salary, and benefits.


3. We use a foreach loop to iterate over the employee names and call the print_employee_info() subroutine for each employee.


4. We calculate the total salary of all employees by summing up their salaries using another foreach loop.


5. We find the employee with the highest salary by iterating over the employees and keeping track of the highest salary and the employee with that salary.


6. We group the employees by department using another foreach loop and a hash of arrays (%employees_by_department).


7. Finally, we print the employees by department using another foreach loop.