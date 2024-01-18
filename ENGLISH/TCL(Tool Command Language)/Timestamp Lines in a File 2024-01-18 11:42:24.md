```tcl

# Define a variable to store the input file's contents
set input_file "input.txt"

# Define a variable to store the output file's contents
set output_file "output.txt"

# Open the input file for reading
open $input_file r

# Create the output file for writing
open $output_file w

# Read lines from the input file and write them to the output file,
# prepending a timestamp to each line
while {[gets $input_file line] != -1} {
    set timestamp [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
    puts $output_file "$timestamp $line"
}

# Close the files
close $input_file
close $output_file

# Print a message to indicate that the process is complete
puts "Timestamped lines written to $output_file"

```

### Explanation:
1. Define variables to store the input and output filenames:
   - `input_file`: Stores the name of the input file.
   - `output_file`: Stores the name of the output file.

2. Open the input file for reading using the `open` command with the `r` flag, which specifies read-only mode.

3. Create the output file for writing using the `open` command with the `w` flag, which specifies write-only mode.

4. Use a `while` loop to read lines from the input file line by line using the `gets` command. The loop continues until there are no more lines to read (i.e., `gets` returns -1).

5. For each line read from the input file, prepend a timestamp to the line using the `clock` and `format` commands. The timestamp is formatted in the "%Y-%m-%d %H:%M:%S" format, which represents the current date and time.

6. Write the timestamped line to the output file using the `puts` command.

7. Close both the input and output files using the `close` command.

8. Print a message to indicate that the process is complete using the `puts` command.

This code reads lines from an input file, prepends a timestamp to each line, and writes the timestamped lines to an output file. The timestamp is added to each line to provide a record of when the line was processed.