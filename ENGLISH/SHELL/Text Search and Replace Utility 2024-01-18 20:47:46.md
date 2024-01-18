```
#!/bin/bash

# Define some colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'
NC='\033[0m' # No Color

# Define some variables
input_file="input.txt"
output_file="output.txt"
temp_file="temp.txt"
search_string="this"
replace_string="that"

# Check if the input file exists
if [ ! -f "$input_file" ]; then
  echo "${RED}Error: Input file $input_file not found.${NC}"
  exit 1
fi

# Create a temporary file for storing the modified content
touch "$temp_file"

# Loop through each line of the input file and replace the search string with the replace string
while read line; do
  # Replace the search string with the replace string
  new_line=${line//$search_string/$replace_string}

  # Write the modified line to the temporary file
  echo "$new_line" >> "$temp_file"
done < "$input_file"

# Copy the contents of the temporary file to the output file
cp "$temp_file" "$output_file"

# Remove the temporary file
rm "$temp_file"

# Display a success message
echo "${GREEN}Successfully replaced '$search_string' with '$replace_string' in $input_file and saved the output to $output_file.${NC}"
```

Explanation:

This script takes an input text file, searches for a specific string within that file, and replaces it with another string. Here's a detailed explanation of the code:

1. Define Colors for Output:

```
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'
NC='\033[0m'
```
These lines define color codes for text output, allowing us to colorize error and success messages.

2. Define Variables:

```
input_file="input.txt"
output_file="output.txt"
temp_file="temp.txt"
search_string="this"
replace_string="that"
```
These variables store the names of the input file, output file, temporary file, the string to search for, and the string to replace it with.

3. Check if the Input File Exists:

```
if [ ! -f "$input_file" ]; then
  echo "${RED}Error: Input file $input_file not found.${NC}"
  exit 1
fi
```
This code checks if the input file exists. If it doesn't, it displays an error message and exits the script with an error code of 1.

4. Create a Temporary File:

```
touch "$temp_file"
```
This line creates a temporary file named "temp.txt." We'll use this file to store the modified content of the input file.

5. Loop Through Input File and Replace Search String:

```
while read line; do
  # Replace the search string with the replace string
  new_line=${line//$search_string/$replace_string}

  # Write the modified line to the temporary file
  echo "$new_line" >> "$temp_file"
done < "$input_file"
```
This code uses a while loop to read each line from the input file. For each line, it replaces the search string with the replace string and stores the modified line in the temporary file.

6. Copy Temporary File to Output File:

```
cp "$temp_file" "$output_file"
```
This line copies the contents of the temporary file to the output file.

7. Remove Temporary File:

```
rm "$temp_file"
```
This line removes the temporary file since we no longer need it.

8. Display Success Message:

```
echo "${GREEN}Successfully replaced '$search_string' with '$replace_string' in $input_file and saved the output to $output_file.${NC}"
```
This line displays a success message indicating that the replacement process was successful.

This script performs a simple text search and replace operation on a given input file, creating an output file with the modified content. It demonstrates the use of loops, file I/O, and string manipulation in a Bash script.