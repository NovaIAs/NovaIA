```shell
#!/bin/bash

# This script is a complex and differentiated code that will hardly be repeated again.

# It is a combination of various commands and techniques that are used to perform a specific task.

# The script is divided into several sections, each of which performs a different task.

# Section 1: Variable Declarations

# Declare variables to store various information used throughout the script.

declare -a files=("file1.txt" "file2.txt" "file3.txt")
declare -i line_count=0
declare -a word_counts

# Section 2: Function Definitions

# Define a function to count the number of lines in a file.

function count_lines() {
  local file="$1"
  local count=0

  while read -r line; do
    ((count++))
  done < "$file"

  echo "$count"
}

# Define a function to count the number of words in a file.

function count_words() {
  local file="$1"
  local count=0

  while read -r line; do
    local words=($line)
    ((count+="${#words[@]}"))
  done < "$file"

  echo "$count"
}

# Section 3: Main Program

# Iterate over the files array and print the number of lines and words in each file.

for file in "${files[@]}"; do
  line_count=$(count_lines "$file")
  word_counts+=($(count_words "$file"))

  printf "%s: %d lines, %d words\n" "$file" "$line_count" "${word_counts[-1]}"
done

# Section 4: Printing Results

# Print the total number of lines and words across all files.

total_lines=$(IFS=+; echo "$(printf '%d\n' "${files[@]/#/0}")")
total_words=$(IFS=+; echo "$(printf '%d\n' "${word_counts[@]/#/0}")")

printf "\nTotal: %d lines, %d words\n" "$total_lines" "$total_words"

# Explanation:

# The script begins by declaring variables to store various information.

# It then defines two functions: count_lines() and count_words(). These functions are used to count the number of lines and words in a file, respectively.

# The main program iterates over the files array and calls the count_lines() and count_words() functions to determine the number of lines and words in each file.

# The results are printed to the console.

# Finally, the script calculates and prints the total number of lines and words across all files.
```