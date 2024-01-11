```perl
use strict;
use warnings;

# Define a custom exception class
package MyException;
use Carp;

sub new {
    my ($class, $message) = @_;
    my $self = bless {
        message => $message,
    }, $class;
    return $self;
}

sub __str__ {
    my $self = shift;
    return $self->{message};
}

package main;

# Define a function that may raise an exception
sub risky_function {
    my ($x, $y) = @_;

    if ($x == 0) {
        croak MyException->new("Division by zero");
    }

    return $y / $x;
}

# Try-catch block to handle exceptions
eval {
    risky_function(0, 10);
};

if ($@) {
    print "An exception occurred: $@\n";
} else {
    print "No exception occurred\n";
}

# Define a subroutine to handle signals
sub handle_signal {
    my $sig = shift;

    print "Signal $sig received\n";
    exit 1;
}

# Install signal handlers
$SIG{INT} = \&handle_signal;
$SIG{TERM} = \&handle_signal;

# Create a new thread
my $thread = threads->new(\&long_running_task);

# Join the thread to the main thread
$thread->join();

sub long_running_task {
    # Simulate a long-running task by sleeping for 10 seconds
    sleep 10;

    print "Long-running task completed\n";
}

# Use a regular expression to parse a string
my $string = "This is a string";
my ($match) = $string =~ /is/;

if ($match) {
    print "The string contains the substring 'is'\n";
} else {
    print "The string does not contain the substring 'is'\n";
}

# Use a module to generate random numbers
use Math::Random;

my $random = Math::Random->new();

my $number = $random->rand();

print "Random number: $number\n";

# Use a module to send email
use Email::Sender;

my $email = Email::Sender->new({
    smtp => 'smtp.example.com',
    port => 587,
    username => 'user@example.com',
    password => 'password',
});

$email->send({
    to => 'recipient@example.com',
    from => 'sender@example.com',
    subject => 'Hello from Perl',
    body => 'This is an email sent from Perl',
});

print "Email sent\n";

# Use a module to parse JSON data
use JSON;

my $json_data = '{
    "name": "John Doe",
    "age": 30,
    "address": {
        "street": "123 Main Street",
        "city": "Anytown",
        "state": "CA",
        "zip": "12345"
    }
}';

my $data = JSON->decode($json_data);

print "Name: $data->{name}\n";
print "Age: $data->{age}\n";
print "Address: $data->{address}{street}\n";
print "City: $data->{address}{city}\n";
print "State: $data->{address}{state}\n";
print "Zip: $data->{address}{zip}\n";
```

This code is a complex and differentiated PERL code that includes a custom exception class, signal handling, threading, regular expressions, random number generation, email sending, JSON parsing, and more. It is a good example of how PERL can be used to write complex and powerful programs.

Here is a brief explanation of the code:

* The `MyException` class is a custom exception class that can be used to handle exceptions in a more structured way.
* The `risky_function()` function is a function that may raise an exception.
* The `try-catch` block is used to handle exceptions that may be raised by the `risky_function()` function.
* The `handle_signal()` subroutine is used to handle signals such as `SIGINT` (Ctrl+C) and `SIGTERM`.
* The `threads` module is used to create a new thread.
* The `Math::Random` module is used to generate random numbers.
* The `Email::Sender` module is used to send emails.
* The `JSON` module is used to parse JSON data.

This code is just a small example of what is possible with PERL. It is a powerful and versatile language that can be used to write a wide variety of programs.