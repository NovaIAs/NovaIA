```
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Define variables
input_file="input.txt"
output_file="output.txt"
temp_file="temp.txt"

# Check if the input file exists
if [ ! -f "$input_file" ]; then
  echo -e "${RED}Error: Input file $input_file does not exist.${NC}"
  exit 1
fi

# Create a temporary file for processing
> "$temp_file"

# Read the input file line by line and process it
while IFS= read -r line; do

  # Split the line into fields
  fields=($line)

  # Check if the line has the correct number of fields
  if [ ${#fields[@]} -ne 3 ]; then
    echo -e "${YELLOW}Warning: Ignoring line $line. Incorrect number of fields.${NC}"
    continue
  fi

  # Get the values from the fields
  name=${fields[0]}
  age=${fields[1]}
  city=${fields[2]}

  # Validate the values
  if [ "$age" -lt 18 ]; then
    echo -e "${RED}Error: Age must be greater than or equal to 18. Ignoring line $line.${NC}"
    continue
  fi

  if [ "$city" == "" ]; then
    echo -e "${RED}Error: City cannot be empty. Ignoring line $line.${NC}"
    continue
  fi

  # Add the processed line to the temporary file
  echo "$name,$age,$city" >> "$temp_file"
done < "$input_file"

# Sort the temporary file by age
sort -n -t "," -k 2 "$temp_file" > "$output_file"

# Remove the temporary file
rm "$temp_file"

# Output a success message
echo -e "${GREEN}Data processed successfully and saved to $output_file.${NC}"
```

This script reads data from an input file, processes it, and saves the processed data to an output file. The script performs the following tasks:

1. It checks if the input file exists. If the file does not exist, it displays an error message and exits.
2. It creates a temporary file for processing the data.
3. It reads the input file line by line and splits each line into fields. It then validates the values in the fields and adds the processed line to the temporary file.
4. It sorts the temporary file by age and saves the sorted data to the output file.
5. It removes the temporary file.
6. It displays a success message.

The script uses the following features of the Bash shell:

* Conditional statements (if, elif, else)
* Loops (while, for)
* File I/O (read, echo, >)
* Regular expressions (IFS=)
* Arrays (${}, ${#array[@]})
* Substrings (${variable:start:length})
* Command substitution (``)

The script also uses the following external commands:

* sort