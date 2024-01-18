1. **Complex String Manipulation:**

```shell
# Remove all punctuation and spaces from a string
cleanedString=$(echo "$string" | tr -dc '[:alnum:]' | tr -d '[:space:]')

# Convert a string to lowercase and replace spaces with underscores
lowercaseUnderscore=$(echo "$string" | tr '[:upper:]' '[:lower:]' | tr ' ' '_')

# Replace multiple spaces with a single space
collapsedSpaces=$(echo "$string" | tr -s '[:space:]')
```
Explanation:

- `tr -dc '[:alnum:]'`: Removes all characters that are not alphanumeric.
- `tr -d '[:space:]'`: Removes all spaces.
- `tr '[:upper:]' '[:lower:]'`: Converts the string to lowercase.
- `tr ' ' '_'`: Replaces spaces with underscores.
- `tr -s '[:space:]'`: Collapses multiple spaces into a single space.

2. **File System Navigation:**

```shell
# Move all files in a directory to another directory
mv /path/to/source/* /path/to/destination

# Copy a directory and all its contents to another location
cp -r /path/to/source /path/to/destination

# Find all files with a specific extension in a directory and its subdirectories
find /path/to/directory -name "*.txt"

# Delete all files with a specific extension in a directory and its subdirectories
find /path/to/directory -name "*.txt" -delete
```
Explanation:

- `mv`: Moves files from one location to another.
- `cp -r`: Copies a directory and its contents recursively.
- `find`: Finds files and directories based on specific criteria.
- `-name "*.txt"`: Specifies the file extension to search for.
- `-delete`: Deletes the files found by the search criteria.

3. **Command Execution:**

```shell
# Run a command and capture its output
output=$(command)

# Run a command in the background
command &

# Run a command and wait for it to finish
command; wait

# Run a command with elevated privileges (requires sudo)
sudo command
```
Explanation:

- `command`: Executes a command.
- `output=$(command)`: Captures the output of the command and assigns it to a variable.
- `command &`: Runs the command in the background.
- `command; wait`: Runs the command and waits for it to finish before proceeding.
- `sudo command`: Runs the command with elevated privileges, requiring the user to enter their password.

4. **Conditionals and Loops:**

```shell
# If-else statement
if [ "$condition" ]; then
  # Commands to execute if the condition is true
else
  # Commands to execute if the condition is false
fi

# While loop
while [ "$condition" ]; do
  # Commands to execute while the condition is true
done

# For loop
for var in "$list"; do
  # Commands to execute for each item in the list
done
```
Explanation:

- `if [ "$condition" ]; then`: Evaluates a condition and executes the following commands if the condition is true.
- `else`: Executes the following commands if the condition is false.
- `while [ "$condition" ]; do`: Executes the following commands repeatedly as long as the condition is true.
- `for var in "$list"; do`: Iterates over a list of items and executes the following commands for each item.

5. **Regular Expressions:**

```shell
# Find lines that contain a specific pattern in a file
grep "pattern" /path/to/file

# Replace all occurrences of a pattern with another pattern in a file
sed -i "s/pattern/replacement/g" /path/to/file

# Extract a specific part of a string using regular expressions
extracted=$(echo "$string" | grep -o "pattern")
```
Explanation:

- `grep`: Searches for lines that match a specific regular expression.
- `sed`: Edits a file by performing search and replace operations based on regular expressions.
- `-i`: Modifies the file in-place.
- `-o`: Only prints the matched part of the line.

6. **Variable Manipulation:**

```shell
# Assign a value to a variable
var="value"

# Append a value to a variable
var+="_suffix"

# Increment a numeric variable
var=$((var + 1))

# Decrement a numeric variable
var=$((var - 1))

# Check if a variable is empty
if [ -z "$var" ]; then
  # Variable is empty
else
  # Variable is not empty
fi
```
Explanation:

- `var="value"`: Assigns the value "value" to the variable `var`.
- `var+="_suffix"`: Appends the value "_suffix" to the variable `var`.
- `var=$((var + 1))`: Increments the numeric value of the variable `var` by 1.
- `var=$((var - 1))`: Decrements the numeric value of the variable `var` by 1.
- `if [ -z "$var" ]; then`: Checks if the variable `var` is empty.

7. **Mathematical Operations:**

```shell
# Perform basic mathematical operations
result=$((5 + 3))  # Addition
result=$((5 - 3))  # Subtraction
result=$((5 * 3))  # Multiplication
result=$((5 / 3))  # Division

# Perform complex mathematical operations using bc
result=$(echo "scale=2; 5.5 + 3.3" | bc)  # Addition with 2 decimal places

# Calculate the square root of a number
result=$(echo "scale=2; sqrt(16)" | bc)  # Square root with 2 decimal places
```
Explanation:

- `result=$((5 + 3))`: Calculates the sum of 5 and 3 and assigns the result to the variable `result`.
- `echo "scale=2; 5.5 + 3.3" | bc`: Uses the `bc` calculator to perform a floating-point addition with 2 decimal places.
- `echo "scale=2; sqrt(16)" | bc`: Uses the `bc` calculator to calculate the square root of 16 with 2 decimal places.