```shell
#!/bin/bash

# Define variables
input_file="input.txt"
output_file="output.txt"
num_lines=$(wc -l < "$input_file")

# Check if input file exists
if [ ! -f "$input_file" ]; then
  echo "Error: Input file $input_file does not exist."
  exit 1
fi

# Create output file if it does not exist
if [ ! -f "$output_file" ]; then
  touch "$output_file"
fi

# Iterate over each line in the input file
line_number=1
while read line; do

  # Skip empty lines
  if [[ -z "$line" ]]; then
    continue
  fi

  # Remove leading and trailing whitespace
  line=$(echo "$line" | xargs)

  # Split line into words
  words=($line)

  # Reverse the order of the words
  reversed_words=($(printf "%s " "${words[@]}" | tac))

  # Append reversed line to output file
  echo "${reversed_words[@]}" >> "$output_file"

  # Increment line number
  line_number=$((line_number + 1))

done < "$input_file"

# Print success message
echo "Reversed lines written to $output_file"
```

Explanation:

1. Define variables:
   - `input_file`: Name of the input file containing the lines to be reversed.
   - `output_file`: Name of the output file where the reversed lines will be written.
   - `num_lines`: Number of lines in the input file.

2. Check if the input file exists. If it doesn't, display an error message and exit the script.

3. Create the output file if it doesn't exist.

4. Use a `while` loop to iterate over each line in the input file.

5. Skip empty lines.

6. Remove leading and trailing whitespace from each line.

7. Split the line into words using the `xargs` command.

8. Reverse the order of the words in the line using the `tac` command.

9. Append the reversed line to the output file.

10. Increment the line number.

11. After processing all lines in the input file, display a success message indicating that the reversed lines have been written to the output file.

This script reads each line from the input file, removes leading and trailing whitespace, splits the line into words, reverses the order of the words, and appends the reversed line to the output file. It skips empty lines and displays a success message upon completion.