```perl
use strict;
use warnings;

my $input_file = 'input.txt';
my $output_file = 'output.txt';

open(my $input_fh, '<', $input_file) or die "Could not open input file: $!";
open(my $output_fh, '>', $output_file) or die "Could not open output file: $!";

my @lines = <$input_fh>;

foreach my $line (@lines) {
    $line =~ s/(\d+)/sprintf("%03d", $1)/ge;
    print $output_fh $line;
}

close($input_fh);
close($output_fh);
```

This Perl script reads a text file, converts all 3-digit numbers in the file to 3-digit numbers with leading zeros, and then writes the modified text to a new file.

Here's a breakdown of the code:

1. **Modules**:
   - `use strict;` and `use warnings;`: These modules help in writing cleaner code by enforcing stricter rules and providing warnings for potential problems.

2. **Input and Output Files**:
   - `my $input_file = 'input.txt';` and `my $output_file = 'output.txt';`: These variables store the paths to the input and output text files.

3. **File Handling**:
   - `open(my $input_fh, '<', $input_file)` and `open(my $output_fh, '>', $output_file)`: These lines open the input and output files for reading and writing, respectively.

4. **Reading the Input File**:
   - `my @lines = <$input_fh>;`: This line reads all lines from the input file and stores them in the array `@lines`.

5. **Processing Each Line**:
   - `foreach my $line (@lines) {...}`: This loop iterates over each line in the array `@lines`.

6. **Regular Expression Substitution**:
   - `$line =~ s/(\d+)/sprintf("%03d", $1)/ge;`: This line uses a regular expression to find and replace all 3-digit numbers (`(\d+)`) in the line with 3-digit numbers with leading zeros (`sprintf("%03d", $1)`).

7. **Writing to the Output File**:
   - `print $output_fh $line;`: This line writes the modified line to the output file.

8. **Closing Files**:
   - `close($input_fh);` and `close($output_fh);`: These lines close the input and output files.