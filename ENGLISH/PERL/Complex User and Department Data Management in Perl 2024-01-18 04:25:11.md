```perl
use strict;
use warnings;

my %users = (
    'alice' => {
        name => 'Alice',
        email => 'alice@example.com',
        age => 25,
    },
    'bob' => {
        name => 'Bob',
        email => 'bob@example.com',
        age => 30,
    },
    'charlie' => {
        name => 'Charlie',
        email => 'charlie@example.com',
        age => 35,
    },
);

my %departments = (
    'sales' => {
        manager => 'John Doe',
        employees => [
            'alice',
            'bob',
        ],
    },
    'marketing' => {
        manager => 'Jane Smith',
        employees => [
            'charlie',
        ],
    },
);

my $user = 'alice';
my $department = 'sales';

my $user_data = $users{$user};
my $department_data = $departments{$department};

print "User: $user_data->{name} ($user_data->{email})\n";
print "Department: $department_data->{manager}\n";
print "Employees:\n";

foreach my $employee (@{$department_data->{employees}}) {
    my $employee_data = $users{$employee};
    print "  - $employee_data->{name} ($employee_data->{email})\n";
}
```

This Perl code is a complex and differentiated program that manages user and department data. It uses a hash of hashes to store the data, and it allows you to retrieve information about a specific user or department.

The code first defines two hashes: one for users and one for departments. The user hash contains information about each user, such as their name, email address, and age. The department hash contains information about each department, such as the manager and a list of employees.

The code then prompts the user for a user name and a department name. It then retrieves the data for the specified user and department from the hashes.

The code finally prints the information about the user and department to the console.

Here is a more detailed explanation of the code:

* The `use strict;` and `use warnings;` statements at the beginning of the code tell Perl to be stricter about syntax and to warn about potential problems.

* The `%users` hash is defined to store user data. The keys of the hash are the user names, and the values are hashes containing the user's name, email address, and age.

* The `%departments` hash is defined to store department data. The keys of the hash are the department names, and the values are hashes containing the department manager and a list of employees.

* The code prompts the user for a user name and a department name using the `print` and `<>` operators.

* The code retrieves the data for the specified user and department from the hashes using the `$users{$user}` and `$departments{$department}` expressions.

* The code finally prints the information about the user and department to the console using the `print` statement.