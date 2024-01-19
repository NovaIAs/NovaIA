```shell
#!/bin/bash

# Check if the user has provided a filename as the first argument.
if [ $# -ne 1 ]; then
  echo "Usage: $0 <filename>"
  exit 1
fi

# Assign the filename to a variable.
filename=$1

# Check if the file exists.
if [ ! -f "$filename" ]; then
  echo "Error: The file \"$filename\" does not exist."
  exit 1
fi

# Create an array to store the lines of the file.
lines=()

# Read the lines of the file into the array.
while read -r line; do
  lines+=("$line")
done < "$filename"

# Create an empty array to store the unique lines of the file.
unique_lines=()

# Loop through the lines of the file.
for line in "${lines[@]}"; do
  # Check if the line is already in the array of unique lines.
  if [[ ! " ${unique_lines[@]} " =~ " $line " ]]; then
    # If the line is not in the array of unique lines, add it to the array.
    unique_lines+=("$line")
  fi
done

# Sort the array of unique lines.
unique_lines=($(sort <<<"${unique_lines[*]}"))

# Print the unique lines of the file.
for line in "${unique_lines[@]}"; do
  echo "$line"
done
```

Explanation:

This code is a shell script that takes a filename as its only argument and prints the unique lines of that file.

The script first checks if the user has provided a filename as the first argument. If not, it prints an error message and exits.

Next, the script assigns the filename to a variable called `filename`.

The script then checks if the file exists. If not, it prints an error message and exits.

The script then creates an array called `lines` to store the lines of the file.

The script then uses a `while` loop to read the lines of the file into the array. The `read -r` command is used to read each line of the file and assign it to the variable `line`. The `lines+=("$line")` command is used to add the line to the `lines` array.

The script then creates an empty array called `unique_lines` to store the unique lines of the file.

The script then uses a `for` loop to loop through the lines of the file. For each line, the script checks if the line is already in the `unique_lines` array using the `[[ ! " ${unique_lines[@]} " =~ " $line " ]]` command. If the line is not in the `unique_lines` array, the script adds it to the array using the `unique_lines+=("$line")` command.

The script then sorts the `unique_lines` array using the `sort` command.

Finally, the script uses a `for` loop to print the unique lines of the file.