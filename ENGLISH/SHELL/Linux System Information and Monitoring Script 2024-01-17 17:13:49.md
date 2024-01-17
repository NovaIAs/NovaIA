```shell
#!/bin/bash

# Define colors to use for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'

# Reset color after output
NC='\033[0m'

# Check if the user is root
if [[ $EUID -ne 0 ]]; then
  echo -e "${RED}This script must be run as root!${NC}"
  exit 1
fi

# Define a function to print a message to the console
function print_message() {
  local message="$1"
  local color="$2"

  echo -e "${color}${message}${NC}"
}

# Print a welcome message
print_message "Welcome to the Linux Administration Script!" $GREEN

# Get the hostname of the system
hostname=$(hostname)

# Print the hostname
print_message "Hostname: $hostname" $BLUE

# Get the IP address of the system
ip_address=$(hostname -I | awk '{print $1}')

# Print the IP address
print_message "IP Address: $ip_address" $BLUE

# Get the operating system version
os_version=$(cat /etc/os-release | grep VERSION | cut -d '=' -f 2 | sed 's/"//g')

# Print the operating system version
print_message "Operating System Version: $os_version" $BLUE

# Get the kernel version
kernel_version=$(uname -r)

# Print the kernel version
print_message "Kernel Version: $kernel_version" $BLUE

# Get the memory usage
memory_usage=$(free -m | grep Mem: | awk '{print $3 "MB /" $2 "MB"}')

# Print the memory usage
print_message "Memory Usage: $memory_usage" $BLUE

# Get the disk space usage
disk_usage=$(df -h | grep /dev/ | awk '{print $5 " " $6}')

# Print the disk space usage
print_message "Disk Space Usage:" $BLUE
echo "$disk_usage"

# Get a list of running processes
running_processes=$(ps aux | grep -v PID | awk '{print $2 " " $11}')

# Print the list of running processes
print_message "Running Processes:" $BLUE
echo "$running_processes"

# Get a list of installed packages
installed_packages=$(dpkg --list | awk '{print $2}')

# Print the list of installed packages
print_message "Installed Packages:" $BLUE
echo "$installed_packages"

# Print a goodbye message
print_message "Goodbye!" $GREEN

```

Explanation:

1. The script starts by defining colors to use for output, making it easier to differentiate different types of messages.

2. It checks if the user is running the script as root. If not, it displays an error message and exits.

3. The script defines a function called `print_message` to print a message to the console with a specified color.

4. It prints a welcome message to the user.

5. The script gathers information about the system, including hostname, IP address, operating system version, kernel version, memory usage, disk space usage, running processes, and installed packages.

6. It prints the gathered information to the console, using different colors for different types of messages.

7. Finally, the script prints a goodbye message to the user.