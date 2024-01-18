```shell
#!/bin/bash

# This is a complex and differentiated code that will hardly be repeated again.

# The code is divided into several sections:

# 1. Initialization
# 2. Functions
# 3. Main Logic
# 4. Output

# 1. Initialization

# Set the default values for the variables.
default_input_file="input.txt"
default_output_file="output.txt"
default_operation="add"

# Read the command-line arguments.
while [[ $# -gt 0 ]]; do
  case $1 in
    -i|--input)
      input_file=$2
      shift # past argument
      shift # past value
      ;;
    -o|--output)
      output_file=$2
      shift # past argument
      shift # past value
      ;;
    -op|--operation)
      operation=$2
      shift # past argument
      shift # past value
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

# Check if the input file exists.
if [[ ! -f $input_file ]]; then
  echo "Input file does not exist: $input_file"
  exit 1
fi

# Check if the output file exists.
if [[ -f $output_file ]]; then
  echo "Output file already exists: $output_file"
  exit 1
fi

# 2. Functions

# Define the add function.
add() {
  echo $1 + $2 | bc
}

# Define the subtract function.
subtract() {
  echo $1 - $2 | bc
}

# Define the multiply function.
multiply() {
  echo $1 * $2 | bc
}

# Define the divide function.
divide() {
  echo $1 / $2 | bc
}

# 3. Main Logic

# Read the input file line by line.
while IFS= read -r line; do

  # Split the line into the operands and the operator.
  operands=(${line// / })
  operator=${operands[1]}

  # Check if the operator is valid.
  if [[ ! $operator =~ [+,-,*,/] ]]; then
    echo "Invalid operator: $operator"
    exit 1
  fi

  # Perform the operation on the operands.
  result=$(case $operator in
    +) add ${operands[0]} ${operands[2]};;
    -) subtract ${operands[0]} ${operands[2]};;
    *) multiply ${operands[0]} ${operands[2]};;
    /) divide ${operands[0]} ${operands[2]};;
  esac)

  # Write the result to the output file.
  echo $result >> $output_file
done < $input_file

# 4. Output

# Display the output file.
cat $output_file

```

This code is a complex and differentiated program that will hardly be repeated again. It is divided into several sections: initialization, functions, main logic, and output.

The initialization section sets the default values for the variables, reads the command-line arguments, and checks if the input and output files exist.

The functions section defines the add, subtract, multiply, and divide functions.

The main logic section reads the input file line by line, splits the line into the operands and the operator, checks if the operator is valid, performs the operation on the operands, and writes the result to the output file.

The output section displays the output file.