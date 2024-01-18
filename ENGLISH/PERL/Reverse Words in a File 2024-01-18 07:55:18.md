```
#!/usr/bin/perl

use strict;    # Enables strict error checking (and warnings if not in the main program)
use warnings;  # Prints warnings, but don't stop if you encounter them

my $input_file_name = 'data.txt';
my $output_file_name = 'output.txt';

# Open the input file
open(INPUT_FILE, '<', $input_file_name) or die "Couldn't open the input file: $!"; 

# Open the output file
open(OUTPUT_FILE, '>', $output_file_name) or die "Couldn't open the output file: $!"; 

# Read the input file line by line
while(my $line = <INPUT_FILE>) {

    # Split the line into words
    my @words = split(/\s+/, $line);

    # Reverse the words in the line
    my @reversed_words = reverse(@words);

    # Join the reversed words back into a line
    my $reversed_line = join(' ', @reversed_words);

    # Write the reversed line to the output file
    print OUTPUT_FILE "$reversed_line\n";

}

# Close the input file
close(INPUT_FILE);

# Close the output file
close(OUTPUT_FILE);
```

Explanation:

1. `#!/usr/bin/perl`: This is a special line that tells the system that this script is a Perl script and should be executed using the Perl interpreter.

2. `use strict;` and `use warnings;`: These lines enable strict error checking and warnings, respectively. This helps catch errors and potential issues early on in the program.

3. `my $input_file_name = 'data.txt';` and `my $output_file_name = 'output.txt';`: These lines assign the names of the input and output files to variables.

4. `open(INPUT_FILE, '<', $input_file_name) or die "Couldn't open the input file: $!";`: This line opens the input file in read mode. If the file cannot be opened, the program prints an error message and exits.

5. `open(OUTPUT_FILE, '>', $output_file_name) or die "Couldn't open the output file: $!";`: This line opens the output file in write mode. If the file cannot be opened, the program prints an error message and exits.

6. `while(my $line = <INPUT_FILE>) {`: This line starts a while loop that will continue until the end of the input file is reached.

7. `my @words = split(/\s+/, $line);`: This line splits the current line of the input file into words based on whitespace characters (spaces, tabs, and newlines) and stores the words in an array called `@words`.

8. `my @reversed_words = reverse(@words);`: This line reverses the order of the words in the `@words` array and stores the reversed array in `@reversed_words`.

9. `my $reversed_line = join(' ', @reversed_words);`: This line joins the reversed words back into a line of text, separated by spaces, and stores it in the variable `$reversed_line`.

10. `print OUTPUT_FILE "$reversed_line\n";`: This line prints the reversed line to the output file, followed by a newline character.

11. `close(INPUT_FILE);` and `close(OUTPUT_FILE);`: These lines close the input and output files, respectively.

This script reads a text file line by line, reverses the words in each line, and writes the reversed lines to an output file.