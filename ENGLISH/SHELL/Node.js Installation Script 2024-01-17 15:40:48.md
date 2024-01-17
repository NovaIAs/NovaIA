```shell
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'
NC='\033[0m' # No Color

# Define functions
function print_error {
  echo -e "${RED}$1${NC}"
}

function print_success {
  echo -e "${GREEN}$1${NC}"
}

function print_warning {
  echo -e "${YELLOW}$1${NC}"
}

function print_info {
  echo -e "${BLUE}$1${NC}"
}

function print_debug {
  echo -e "${MAGENTA}$1${NC}"
}

# Check if a command exists
function command_exists {
  command -v $1 >/dev/null 2>&1
}

# Ask for user input
function ask_user {
  read -p "$1 [y/n] " answer
  case $answer in
    y | Y)
      return 0
      ;;
    n | N)
      return 1
      ;;
    *)
      print_error "Invalid input. Please enter 'y' or 'n'."
      ask_user "$1"
      ;;
  esac
}

# Main script starts here
print_info "This script will install the latest version of Node.js."

# Check if Node.js is already installed
if command_exists node; then
  print_info "Node.js is already installed."
  exit 0
fi

# Ask the user if they want to continue
if ! ask_user "Do you want to install Node.js?"; then
  exit 0
fi

# Download and install Node.js
curl -sL https://nodejs.org/dist/latest/node-v*.tar.xz | tar -xJ
cd node-*
./configure
make
make install

# Check if Node.js was installed successfully
if ! command_exists node; then
  print_error "Node.js was not installed successfully."
  exit 1
fi

# Print a success message
print_success "Node.js was installed successfully."

# Exit the script
exit 0
```

This code is a shell script that installs the latest version of Node.js on a Linux system. It first checks if Node.js is already installed, and if it is, it exits the script. If Node.js is not installed, it asks the user if they want to install it. If the user says yes, it downloads and installs Node.js. Finally, it checks if Node.js was installed successfully and prints a success message.

The code is written in a modular style, with each function performing a specific task. This makes the code easier to read and maintain. The code also uses color output to make it more visually appealing.

Here is a breakdown of the code:

* The first few lines of the code define colors for output. These colors are used to print error, success, warning, info, and debug messages.
* The next few lines define functions that are used throughout the script. These functions include:
    * `print_error`: Prints an error message in red.
    * `print_success`: Prints a success message in green.
    * `print_warning`: Prints a warning message in yellow.
    * `print_info`: Prints an info message in blue.
    * `print_debug`: Prints a debug message in magenta.
    * `command_exists`: Checks if a command exists.
    * `ask_user`: Asks the user for input.
* The main script starts by printing an info message to the user.
* It then checks if Node.js is already installed. If it is, it prints an info message and exits the script.
* If Node.js is not installed, it asks the user if they want to install it. If the user says yes, it downloads and installs Node.js.
* Finally, it checks if Node.js was installed successfully and prints a success message.

The code is well-commented and easy to follow. It is also modular and reusable.