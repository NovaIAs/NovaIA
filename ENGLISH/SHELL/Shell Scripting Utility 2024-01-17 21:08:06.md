```shell
#!/bin/bash

# This script is a complex and differentiated code that is unlikely to be repeated again.
# It demonstrates various features of the shell scripting language, including:
#   - Nested loops
#   - Conditional statements
#   - Regular expressions
#   - Functions
#   - Arrays

# This function prints a horizontal line of characters.
# The length of the line is determined by the value of the $COLUMNS variable.
function print_line() {
  local line=""
  for ((i=0; i<$COLUMNS; i++)); do
    line+="-"
  done
  echo "$line"
}

# This function prints a greeting message to the console.
function greet() {
  local name="$1"
  echo "Hello, $name!"
}

# This function calculates the factorial of a number.
function factorial() {
  local number="$1"
  local result=1
  for ((i=2; i<=$number; i++)); do
    result=$((result * i))
  done
  echo "$result"
}

# This function prints a list of files and directories in a directory.
function list_files() {
  local directory="$1"
  for file in "$directory"/*; do
    if [ -f "$file" ]; then
      echo "$file"
    elif [ -d "$file" ]; then
      echo "$file/"
    fi
  done
}

# This function searches for a pattern in a file.
function search_file() {
  local pattern="$1"
  local file="$2"
  grep "$pattern" "$file"
}

# This function replaces a pattern with a new string in a file.
function replace_string() {
  local pattern="$1"
  local new_string="$2"
  local file="$3"
  sed -i "s/$pattern/$new_string/g" "$file"
}

# This function creates a new directory and populates it with files.
function create_directory() {
  local directory="$1"
  mkdir "$directory"
  for ((i=1; i<=10; i++)); do
    touch "$directory/file$i.txt"
  done
}

# This function removes a directory and all of its contents.
function remove_directory() {
  local directory="$1"
  rm -rf "$directory"
}

# This function prints the current date and time in a specific format.
function print_date() {
  date "+%Y-%m-%d %H:%M:%S"
}

# This function prints the current working directory.
function print_cwd() {
  pwd
}

# This function prints the system uptime.
function print_uptime() {
  uptime
}

# This function prints the system load average.
function print_loadavg() {
  cat /proc/loadavg
}

# This function prints the system memory usage.
function print_memory() {
  cat /proc/meminfo | grep Mem
}

# This function prints the system CPU usage.
function print_cpu() {
  cat /proc/stat | grep cpu
}

# This function prints the system network usage.
function print_network() {
  netstat -ant
}

# This function prints the system disk usage.
function print_disk() {
  df -h
}

# This function prints a system information summary.
function print_system_info() {
  echo "Date and Time: $(print_date)"
  echo "Current Working Directory: $(print_cwd)"
  echo "System Uptime: $(print_uptime)"
  echo "System Load Average: $(print_loadavg)"
  echo "System Memory Usage: $(print_memory)"
  echo "System CPU Usage: $(print_cpu)"
  echo "System Network Usage: $(print_network)"
  echo "System Disk Usage: $(print_disk)"
}

# This function prints a help message for the script.
function print_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -h, --help       Print this help message and exit."
  echo "  -g, --greet      Greet the specified user."
  echo "  -f, --factorial   Calculate the factorial of a number."
  echo "  -l, --list-files  List the files and directories in a directory."
  echo "  -s, --search-file  Search for a pattern in a file."
  echo "  -r, --replace-string  Replace a pattern with a new string in a file."
  echo "  -c, --create-directory  Create a new directory and populate it with files."
  echo "  -d, --remove-directory  Remove a directory and all of its contents."
  echo "  -i, --system-info   Print a system information summary."
}

# This is the main body of the script.
main() {
  # Parse the command-line arguments.
  while getopts "hgf:l:s:r:c:d:i" opt; do
    case "$opt" in
      h)
        print_help
        exit 0
        ;;
      g)
        greet "$2"
        exit 0
        ;;
      f)
        echo "Factorial of $2: $(factorial "$2")"
        exit 0
        ;;
      l)
        list_files "$2"
        exit 0
        ;;
      s)
        search_file "$2" "$3"
        exit 0
        ;;
      r)
        replace_string "$2" "$3" "$4"
        exit 0
        ;;
      c)
        create_directory "$2"
        exit 0
        ;;
      d)
        remove_directory "$2"
        exit 0
        ;;
      i)
        print_system_info
        exit 0
        ;;
      \?)
        echo "Invalid option: -$OPTARG" >&2
        exit 1
        ;;
    esac