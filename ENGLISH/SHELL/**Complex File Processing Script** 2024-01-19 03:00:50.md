```shell
#!/bin/bash

# This script performs a series of complex operations on a set of input files.

# Define the input and output directories.
input_dir="input_files"
output_dir="output_files"

# Make sure the output directory exists.
mkdir -p "$output_dir"

# Loop through each file in the input directory.
for file in "$input_dir"/*; do

  # Get the filename without the extension.
  filename=$(basename "$file" .csv)

  # Create the output file.
  output_file="$output_dir/$filename.txt"

  # Perform a series of operations on the file.
  grep -v "," "$file" | # Remove all lines containing commas.
  sort | # Sort the lines in alphabetical order.
  uniq -c | # Count the number of occurrences of each line.
  sort -rn | # Sort the lines in reverse numerical order.
  head -10 > "$output_file" # Keep only the top 10 lines.

done

# Print a success message.
echo "The output files have been generated in the $output_dir directory."
```

Explanation of the code:

* The first line of the code specifies the shell interpreter to be used. In this case, it is Bash.
* The second line defines the input and output directories.
* The third line creates the output directory if it does not already exist.
* The fourth line loops through each file in the input directory.
* The fifth line gets the filename without the extension.
* The sixth line creates the output file.
* The seventh line performs a series of operations on the file. The operations are:
    * `grep -v "," "$file"`: This command removes all lines containing commas from the file.
    * `sort`: This command sorts the lines in alphabetical order.
    * `uniq -c`: This command counts the number of occurrences of each line.
    * `sort -rn`: This command sorts the lines in reverse numerical order.
    * `head -10`: This command keeps only the top 10 lines.
* The eighth line prints a success message.