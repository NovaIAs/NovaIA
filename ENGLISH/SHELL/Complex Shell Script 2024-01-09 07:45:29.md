```shell
#!/bin/bash

# Define some colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'

# Define a function to print a message in a specific color
function print_color() {
  COLOR=$1
  MESSAGE=$2
  echo -e "$COLOR$MESSAGE"
}

# Print a welcome message
print_color GREEN "Welcome to the Complex Shell Script!"

# Define a variable to store the current working directory
CWD=$(pwd)

# Print the current working directory
print_color CYAN "Current working directory: $CWD"

# Create a new directory called "tmp"
mkdir tmp

# Move to the "tmp" directory
cd tmp

# Create a new file called "test.txt"
touch test.txt

# Write some text to the "test.txt" file
echo "This is a test file." > test.txt

# Print the contents of the "test.txt" file
print_color BLUE "Contents of test.txt:"
cat test.txt

# Remove the "test.txt" file
rm test.txt

# Remove the "tmp" directory
rmdir tmp

# Move back to the original working directory
cd $CWD

# Print a goodbye message
print_color RED "Goodbye!"

# Exit the script
exit 0
```

This script is a complex shell script that performs a variety of tasks, including:

* Defining colors for output
* Defining a function to print a message in a specific color
* Printing a welcome message
* Storing the current working directory in a variable
* Printing the current working directory
* Creating a new directory called "tmp"
* Moving to the "tmp" directory
* Creating a new file called "test.txt"
* Writing some text to the "test.txt" file
* Printing the contents of the "test.txt" file
* Removing the "test.txt" file
* Removing the "tmp" directory
* Moving back to the original working directory
* Printing a goodbye message
* Exiting the script

This script is a good example of how to use shell scripting to perform complex tasks.