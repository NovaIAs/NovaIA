```tcl
# This Tcl script demonstrates advanced programming techniques and constructs.

# Define a custom command to calculate factorials.
proc factorial {n} {
    if {$n == 0} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}

# Define a procedure to calculate Fibonacci numbers.
proc fibonacci {n} {
    if {$n <= 1} {
        return $n
    }
    return [expr {[fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]}]
}

# Create a list of numbers from 1 to 10.
set numbers [lrange [list 1 2 3 4 5 6 7 8 9 10] 0 end]

# Use the "foreach" command to iterate over the list and print each number.
foreach number $numbers {
    puts $number
}

# Create a dictionary to store key-value pairs.
dict set my_dict {
    name "John Doe"
    age 30
    city "New York"
}

# Retrieve the value associated with the "name" key from the dictionary.
set name [dict get my_dict name]

# Print the retrieved value.
puts $name

# Create a list of strings.
set strings [list "Hello" "World" "Tcl" "Scripting"]

# Use the "join" command to concatenate the strings into a single string.
set joined_string [join $strings " "]

# Print the joined string.
puts $joined_string

# Create a simple regular expression pattern.
set pattern [regexp {^[A-Z][a-z]{2,}$}]

# Use the "regexp" command to match the pattern against a string.
set result [regexp $pattern "Hello, world!"]

# Check if the match was successful.
if {$result} {
    puts "The string matches the pattern."
} else {
    puts "The string does not match the pattern."
}

# Create a channel for reading from a file.
set file [open "data.txt" "r"]

# Read the entire contents of the file into a variable.
set data [read $file]

# Close the file channel.
close $file

# Print the contents of the file.
puts $data

# Create a channel for writing to a file.
set file [open "output.txt" "w"]

# Write a string to the file.
puts $file "This is a test."

# Close the file channel.
close $file

# Exit the Tcl interpreter.
exit
```

Explanation:

1. `factorial` and `fibonacci` Procedures:
   - These procedures define custom commands to calculate factorials and Fibonacci numbers, respectively.

2. `foreach` Loop:
   - It iterates over a list and prints each element.

3. Dictionary (`dict`):
   - It creates a dictionary with key-value pairs and retrieves a value based on a key.

4. `join` Command:
   - It concatenates a list of strings into a single string.

5. Regular Expression (`regexp`):
   - It matches a string against a regular expression pattern.

6. File I/O:
   - It reads the contents of a file and writes a string to a file.

7. `exit` Command:
   - It exits the Tcl interpreter.