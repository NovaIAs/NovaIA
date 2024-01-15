```shell
#!/bin/bash

# This script is a complex example of how to use shell scripting to perform various tasks.

# Define some variables.
var1="Hello"
var2="World"
var3=123

# Print the values of the variables.
echo "var1 = $var1"
echo "var2 = $var2"
echo "var3 = $var3"

# Perform some calculations.
result=$((var3 + 10))

# Print the result of the calculations.
echo "result = $result"

# Check if a file exists.
if [ -f /etc/passwd ]; then
  echo "/etc/passwd exists"
else
  echo "/etc/passwd does not exist"
fi

# Loop through a list of files.
for file in /etc/*; do
  echo "Processing file: $file"
done

# Create a function.
function my_function() {
  echo "This is a function."
}

# Call the function.
my_function

# Exit the script.
exit 0
```

Explanation:

* The first line of the script specifies the shell interpreter to be used, which is Bash in this case.
* The next few lines define some variables and print their values.
* The line `result=$((var3 + 10))` performs a calculation and stores the result in the variable `result`.
* The line `echo "result = $result"` prints the value of the variable `result`.
* The line `if [ -f /etc/passwd ]; then` checks if the file `/etc/passwd` exists.
* The line `echo "/etc/passwd exists"` is executed if the file exists.
* The line `echo "/etc/passwd does not exist"` is executed if the file does not exist.
* The line `for file in /etc/*; do` starts a loop that iterates through all the files in the directory `/etc`.
* The line `echo "Processing file: $file"` is executed for each file in the loop.
* The line `function my_function() {` defines a function called `my_function`.
* The line `echo "This is a function."` is the body of the function.
* The line `my_function` calls the function.
* The line `exit 0` exits the script with a status code of 0, which indicates that the script completed successfully.