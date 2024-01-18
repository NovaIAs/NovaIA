```perl
use strict;
use warnings;

# Define the main subroutine
sub main {
    # Declare an array of numbers
    my @numbers = (1, 2, 3, 4, 5);

    # Iterate over the array using a foreach loop
    foreach my $number (@numbers) {
        # Check if the number is even
        if ($number % 2 == 0) {
            # If it's even, print "Even"
            print "$number is even\n";
        }
        # Otherwise, print "Odd"
        else {
            print "$number is odd\n";
        }
    }
}

# Call the main subroutine
main();
```

**Explanation:**

1. We start by declaring an array of numbers using the `@numbers` array variable.


2. We then use a `foreach` loop to iterate over the array. The `foreach` loop assigns each element of the array to the `$number` variable in turn.


3. Inside the loop, we use the `if` statement to check if the number is even. We do this by checking if the remainder of the number divided by 2 is equal to 0.


4. If the number is even, we print "Even" to the console using the `print` function.


5. If the number is not even, we print "Odd" to the console using the `print` function.


6. Finally, we call the `main` subroutine to run the program.