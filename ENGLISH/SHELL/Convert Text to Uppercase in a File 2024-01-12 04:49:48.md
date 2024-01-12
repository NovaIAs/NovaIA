```shell
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Define functions
function print_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -h, --help        Display this help message"
  echo "  -v, --verbose     Enable verbose output"
  echo "  -f, --file        Specify the input file"
  echo "  -o, --output       Specify the output file"
}

function process_file() {
  local input_file=$1
  local output_file=$2

  # Check if the input file exists
  if [ ! -f "$input_file" ]; then
    echo "${RED}Error: Input file $input_file does not exist${NC}"
    exit 1
  fi

  # Check if the output file exists
  if [ -f "$output_file" ]; then
    echo "${YELLOW}Warning: Output file $output_file already exists. Overwriting.${NC}"
  fi

  # Open the input file
  exec 3< "$input_file"

  # Open the output file
  exec 4> "$output_file"

  # Read each line from the input file
  while read -u 3 line; do
    # Process the line
    processed_line=$(echo "$line" | tr 'a-z' 'A-Z')

    # Write the processed line to the output file
    echo "$processed_line" >&4
  done

  # Close the input file
  exec 3<&-

  # Close the output file
  exec 4>&-
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help)
      print_help
      exit 0
      ;;
    -v|--verbose)
      verbose=true
      ;;
    -f|--file)
      input_file=$2
      shift
      ;;
    -o|--output)
      output_file=$2
      shift
      ;;
    *)
      echo "${RED}Error: Invalid option $1${NC}"
      print_help
      exit 1
      ;;
  esac
  shift
done

# Check if the input file was specified
if [ -z "$input_file" ]; then
  echo "${RED}Error: Input file not specified${NC}"
  print_help
  exit 1
fi

# Check if the output file was specified
if [ -z "$output_file" ]; then
  echo "${RED}Error: Output file not specified${NC}"
  print_help
  exit 1
fi

# Process the file
process_file "$input_file" "$output_file"

# Print success message
echo "${GREEN}File processed successfully${NC}"
```

This code is a shell script that takes an input file, converts all the lowercase letters in the file to uppercase letters, and saves the result in an output file. The script also includes a help message that can be displayed by passing the `-h` or `--help` flags, and a verbose mode that can be enabled by passing the `-v` or `--verbose` flags.

The script uses the following functions:

* `print_help()`: Displays the help message.
* `process_file()`: Processes the input file and saves the result in the output file.

The script also uses the following variables:

* `input_file`: The path to the input file.
* `output_file`: The path to the output file.
* `verbose`: A flag that indicates whether verbose mode is enabled.

The script first parses the command-line arguments and sets the values of the `input_file`, `output_file`, and `verbose` variables.

The script then checks if the input file and output file were specified. If either file was not specified, the script displays an error message and exits.

The script then calls the `process_file()` function to process the input file and save the result in the output file.

Finally, the script prints a success message.