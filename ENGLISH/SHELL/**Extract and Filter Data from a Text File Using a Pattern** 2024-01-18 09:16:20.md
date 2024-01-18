```shell
#!/bin/bash

# This script is a complex and differentiated code that will hardly be repeated again. It is written in English and provides a detailed explanation of the code.

# Set up variables
input_file="input.txt"
output_file="output.txt"
temporary_file="temporary.txt"
pattern="pattern_to_search"

# Check if the input file exists
if [ ! -f "$input_file" ]; then
  echo "Error: Input file $input_file does not exist."
  exit 1
fi

# Check if the output file exists
if [ -f "$output_file" ]; then
  echo "Error: Output file $output_file already exists."
  exit 1
fi

# Create a temporary file
touch "$temporary_file"

# Read the input file line by line and search for the pattern
while read line; do
  # Check if the line contains the pattern
  if [[ "$line" =~ "$pattern" ]]; then
    # If the line contains the pattern, write it to the temporary file
    echo "$line" >> "$temporary_file"
  fi
done < "$input_file"

# Sort the temporary file
sort "$temporary_file" > "$output_file"

# Remove the temporary file
rm "$temporary_file"

# Print a success message
echo "Pattern $pattern found in $input_file and saved to $output_file."

```

**Explanation:**

1. **Set up variables:**
   - `input_file`: The name of the input file containing the data to be processed.
   - `output_file`: The name of the output file where the results will be saved.
   - `temporary_file`: The name of a temporary file used for intermediate processing.
   - `pattern`: The pattern or string to search for in the input file.

2. **Check if the input file exists:**
   - This step checks if the input file exists using the `-f` operator. If the file does not exist, an error message is displayed, and the script exits with a status code of 1 to indicate an error.

3. **Check if the output file exists:**
   - This step checks if the output file already exists using the `-f` operator. If the file already exists, an error message is displayed, and the script exits with a status code of 1 to indicate an error.

4. **Create a temporary file:**
   - This step creates a temporary file using the `touch` command. This file will be used to store intermediate data during processing.

5. **Read the input file line by line and search for the pattern:**
   - The `while` loop reads the input file line by line using the `read` command.
   - For each line, the `if` statement checks if the line contains the specified pattern using the `[[ ... =~ ... ]]` operator.
   - If the line contains the pattern, it is appended to the temporary file using the `>>` operator.

6. **Sort the temporary file:**
   - After processing all the lines in the input file, the temporary file contains the lines that match the pattern.
   - The `sort` command is used to sort the lines in alphabetical order.

7. **Save the sorted lines to the output file:**
   - The sorted lines are saved to the output file using the `>` operator.

8. **Remove the temporary file:**
   - The temporary file is no longer needed, so it is removed using the `rm` command.

9. **Print a success message:**
   - Finally, a success message is displayed indicating that the pattern has been found in the input file and saved to the output file.