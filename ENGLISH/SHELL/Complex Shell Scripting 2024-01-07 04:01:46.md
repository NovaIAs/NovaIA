```shell
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'
NC='\033[0m' # No Color

# Define a function to print a message with a specific color
print_color() {
  printf "%b%s%b\n" "$1" "$2" "$NC"
}

# Print a welcome message
print_color GREEN "Welcome to the Complex Shell Script!"

# Define a variable to store the current directory
current_dir=$(pwd)

# Print the current directory
print_color BLUE "Current Directory: ${current_dir}"

# Create a new directory
mkdir new_directory

# Move into the new directory
cd new_directory

# Print the current directory
print_color BLUE "Current Directory: $(pwd)"

# Create a file with some content
echo "Hello World!" > hello_world.txt

# Print the contents of the file
print_color YELLOW "Contents of hello_world.txt:"
cat hello_world.txt

# Copy the file to another location
cp hello_world.txt ../copied_hello_world.txt

# Print a message indicating the file was copied
print_color GREEN "File copied to ../copied_hello_world.txt"

# Move back to the original directory
cd ..

# Print the current directory
print_color BLUE "Current Directory: $(pwd)"

# Remove the new directory
rmdir new_directory

# Print a message indicating the directory was removed
print_color RED "Directory 'new_directory' removed"

# Print a goodbye message
print_color PURPLE "Goodbye!"
```

**Explanation:**

* The script starts by defining colors for output using ANSI escape codes. This allows us to print messages in different colors to make the output more visually appealing.

* Next, the script defines a function called `print_color()` which takes two arguments: the color to use and the message to print. This function uses the `printf` command to print the message in the specified color.

* The script then prints a welcome message using the `print_color()` function.

* The script defines a variable called `current_dir` to store the current directory.

* The script prints the current directory using the `print_color()` function.

* The script creates a new directory called "new_directory".

* The script moves into the new directory using the `cd` command.

* The script prints the current directory again using the `print_color()` function.

* The script creates a file called "hello_world.txt" with the content "Hello World!".

* The script prints the contents of the file using the `cat` command.

* The script copies the file to another location called "../copied_hello_world.txt" using the `cp` command.

* The script prints a message indicating that the file was copied.

* The script moves back to the original directory using the `cd` command.

* The script prints the current directory again using the `print_color()` function.

* The script removes the new directory using the `rmdir` command.

* The script prints a message indicating that the directory was removed.

* Finally, the script prints a goodbye message using the `print_color()` function.