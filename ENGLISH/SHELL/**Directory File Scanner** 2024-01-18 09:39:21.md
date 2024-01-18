```
#!/bin/bash

# Define the colors we'll be using
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'

# Define the functions we'll be using
function print_error {
  echo -e "${RED}Error: $1${WHITE}"
}

function print_warning {
  echo -e "${YELLOW}Warning: $1${WHITE}"
}

function print_info {
  echo -e "${BLUE}Info: $1${WHITE}"
}

function print_success {
  echo -e "${GREEN}Success: $1${WHITE}"
}

# Get the current directory
cwd=$(pwd)

# Check if the user has provided a directory to scan
if [ $# -eq 0 ]; then
  print_error "Usage: $0 <directory>"
  exit 1
fi

# Check if the directory exists
if [ ! -d "$1" ]; then
  print_error "Directory $1 does not exist"
  exit 1
fi

# Change to the directory we want to scan
cd "$1"

# Find all the files in the current directory and its subdirectories
files=$(find . -type f)

# Loop through each file and print its name and size
for file in $files; do
  size=$(stat -c%s "$file")
  printf "%-50s %10s bytes\n" "$file" "$size"
done

# Change back to the original directory
cd "$cwd"
```

This script is designed to scan a directory and its subdirectories for all regular files and print their names and sizes. It uses the `find` command to find all the files in the directory tree, and then uses the `stat` command to get the size of each file. The script also uses the `printf` command to format the output so that the file names and sizes are aligned.

Here is a more detailed explanation of the script:

* The first few lines of the script define the colors that will be used to print messages to the console. This is done using the `RED`, `GREEN`, `YELLOW`, `BLUE`, `MAGENTA`, `CYAN`, and `WHITE` variables.
* The next few lines define the functions that will be used by the script. These functions include `print_error()`, `print_warning()`, `print_info()`, and `print_success()`. These functions are used to print messages to the console in different colors and formats.
* The next line gets the current directory using the `pwd` command. This is stored in the `cwd` variable.
* The next line checks if the user has provided a directory to scan. If no directory is provided, the script prints an error message and exits.
* The next line checks if the directory exists. If the directory does not exist, the script prints an error message and exits.
* The next line changes to the directory that we want to scan. This is done using the `cd` command.
* The next line uses the `find` command to find all the files in the current directory and its subdirectories. The `-type f` option is used to only find regular files. The output of the `find` command is stored in the `files` variable.
* The next line loops through each file in the `files` variable. For each file, the script uses the `stat` command to get the size of the file. The size is stored in the `size` variable.
* The next line uses the `printf` command to format the output so that the file names and sizes are aligned. The `%-50s` format specifier is used to left-align the file name and the `%10s` format specifier is used to right-align the file size.
* The last line changes back to the original directory using the `cd` command.