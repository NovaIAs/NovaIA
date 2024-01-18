```shell
#!/bin/bash

# Define some colors for output
RED="\e[31m"
GREEN="\e[32m"
YELLOW="\e[33m"
BLUE="\e[34m"
MAGENTA="\e[35m"
CYAN="\e[36m"
WHITE="\e[37m"

# Define a function to print a message in a specific color
print_color() {
  COLOR=$1
  MESSAGE=$2
  echo -e "${COLOR}${MESSAGE}${WHITE}"
}

# Print a welcome message
print_color GREEN "Welcome to the complex shell script!"

# Get the current date and time
DATE=$(date +%Y-%m-%d)
TIME=$(date +%H:%M:%S)

# Print the current date and time
print_color BLUE "Current date: ${DATE}"
print_color BLUE "Current time: ${TIME}"

# Create a temporary directory
TMPDIR=$(mktemp -d)

# Print the temporary directory
print_color YELLOW "Temporary directory: ${TMPDIR}"

# Copy a file to the temporary directory
cp /etc/passwd ${TMPDIR}

# Print the contents of the file
print_color MAGENTA "Contents of /etc/passwd:"
cat ${TMPDIR}/passwd

# Delete the temporary directory
rm -rf ${TMPDIR}

# Print a goodbye message
print_color RED "Goodbye!"
```

This code creates a temporary directory, copies a file to that directory, prints the contents of the file, and then deletes the directory. It also includes a function for printing messages in a specific color, and uses that function to print various messages throughout the script, making it easier to read and understand.

Here is a more detailed explanation of the code:

* The first few lines define some colors for output. These colors are used by the `print_color()` function to print messages in a specific color.
* The `print_color()` function takes two arguments: the color to use and the message to print. It uses the `echo` command to print the message, and the `-e` option to enable escape sequences. The escape sequence `${COLOR}` is used to set the color of the output, and the escape sequence `${WHITE}` is used to reset the color to white.
* The `Welcome to the complex shell script!` message is printed in green.
* The current date and time are obtained using the `date` command. The `+%Y-%m-%d` format string is used to format the date, and the `+%H:%M:%S` format string is used to format the time.
* The current date and time are printed in blue.
* A temporary directory is created using the `mktemp -d` command. The `-d` option tells `mktemp` to create a directory instead of a file.
* The temporary directory is printed in yellow.
* The `/etc/passwd` file is copied to the temporary directory using the `cp` command.
* The contents of the `/etc/passwd` file are printed in magenta.
* The temporary directory is deleted using the `rm -rf` command. The `-r` option tells `rm` to recursively delete the directory and all of its contents. The `-f` option tells `rm` to force the deletion, even if the directory is not empty.
* The `Goodbye!` message is printed in red.