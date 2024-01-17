```
#!/bin/bash

# This script is a complex example of shell scripting, demonstrating various programming techniques and features.

# Define global variables
input_file="input.txt"
output_file="output.txt"
delimiter=","
sort_order="ascending"

# Define functions
function usage() {
    echo "Usage: $0 [options] input_file output_file"
    echo "Options:"
    echo "  -d delimiter"
    echo "  -s sort_order (ascending or descending)"
    exit 1
}

function sort_data() {
    local input_file=$1
    local output_file=$2
    local delimiter=$3
    local sort_order=$4

    # Read the input file and split each line into an array
    while read -r line; do
        IFS=$delimiter read -ra fields <<< "$line"

        # Sort the array
        if [[ "$sort_order" == "ascending" ]]; then
            sort -t "$delimiter" -k1,1 <<< "${fields[*]}"
        else
            sort -t "$delimiter" -k1,1r <<< "${fields[*]}"
        fi

        # Write the sorted array back to the output file
        echo "${fields[*]}" >> "$output_file"
    done < "$input_file"
}

# Parse command-line arguments
while getopts "d:s:" opt; do
    case $opt in
        d)
            delimiter="$OPTARG"
            ;;
        s)
            sort_order="$OPTARG"
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

# Check if the required number of arguments were provided
if [[ $# -ne 2 ]]; then
    usage
fi

# Call the sort_data function to sort the input file
sort_data "$input_file" "$output_file" "$delimiter" "$sort_order"

# Display the sorted data
echo "Sorted data:"
cat "$output_file"

```

Explanation:

1. **Global Variables**:
   - `input_file`: Name of the input file containing the data to be sorted.
   - `output_file`: Name of the output file where the sorted data will be written.
   - `delimiter`: Character used to separate fields in the input data.
   - `sort_order`: Order in which the data should be sorted (ascending or descending).

2. **Functions**:
   - `usage()`: Displays usage information and exits the script.
   - `sort_data()`: This function reads the input data line by line, splits each line into an array, sorts the array, and writes the sorted array back to the output file.

3. **Command-Line Argument Parsing**:
   - The script uses `getopts` to parse command-line arguments. Allowed options are `-d` for specifying the delimiter and `-s` for specifying the sort order.

4. **Main Logic**:
   - The script checks if the required number of arguments were provided. If not, it displays usage information and exits.
   - It then calls the `sort_data` function to sort the input file and save the sorted data to the output file.
   - Finally, it displays the sorted data on the console.

This script demonstrates the use of functions, command-line argument parsing, and data manipulation in a shell script. It provides a comprehensive example of how to write complex and reusable code in bash.