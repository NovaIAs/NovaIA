```tcl
# This is a complex TCL script that demonstrates various features of the language.

# Create a hash table to store key-value pairs.
dict set hashTable {}

# Loop through a range of numbers and add key-value pairs to the hash table.
for {set i 0} {$i < 10} {incr i} {
    # Generate a random key.
    set key [format "key%03d" $i]

    # Generate a random value.
    set value [format "value%03d" $i]

    # Add the key-value pair to the hash table.
    dict set hashTable($key) $value
}

# Print the contents of the hash table.
dict for {key value} $hashTable {
    puts "Key: $key, Value: $value"
}

# Create a list of numbers.
set list {1 2 3 4 5 6 7 8 9 10}

# Loop through the list and print each number.
foreach num $list {
    puts "Number: $num"
}

# Create a procedure to calculate the factorial of a number.
proc factorial {num} {
    if {$num == 0} {
        return 1
    } else {
        return [expr {$num * [factorial [expr {$num - 1}]]}]
    }
}

# Print the factorial of 5.
puts "Factorial of 5: [factorial 5]"

# Create a regular expression to match email addresses.
set emailRegex "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

# Check if a given string matches the email address regular expression.
if {[regexp $emailRegex "john.doe@example.com"]} {
    puts "Valid email address"
} else {
    puts "Invalid email address"
}

# Create a channel for reading from standard input.
set stdin [open stdin r]

# Read a line of input from standard input.
set line [read $stdin]

# Print the line of input.
puts "Line of input: $line"

# Close the channel for reading from standard input.
close $stdin

# Create a channel for writing to standard output.
set stdout [open stdout w]

# Write a line of output to standard output.
puts $stdout "Hello, world!"

# Close the channel for writing to standard output.
close $stdout
```

Explanation:

This script demonstrates various features of the TCL language, including:

* **Hash tables:** Hash tables are used to store key-value pairs. The code creates a hash table, adds key-value pairs to it, and then prints the contents of the hash table.
* **Lists:** Lists are used to store sequences of elements. The code creates a list of numbers, loops through the list, and prints each number.
* **Procedures:** Procedures are used to define reusable blocks of code. The code defines a procedure to calculate the factorial of a number.
* **Regular expressions:** Regular expressions are used to match patterns in strings. The code defines a regular expression to match email addresses and then checks if a given string matches the regular expression.
* **Channels:** Channels are used to read from and write to files or other devices. The code creates a channel for reading from standard input, reads a line of input, and then prints the line of input. The code also creates a channel for writing to standard output, writes a line of output, and then closes the channel.