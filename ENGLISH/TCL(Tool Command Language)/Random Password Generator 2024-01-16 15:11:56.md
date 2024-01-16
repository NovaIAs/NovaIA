```tcl
# This script generates a random password of a specified length.

# Define the characters to use in the password.
set charSet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()"

# Get the desired password length from the user.
puts "Enter the desired password length:"
set length [gets stdin]

# Generate a random password.
set password ""
for {set i 0} {$i < $length} {incr i} {
    # Generate a random index into the character set.
    set index [expr {int(rand() * [llength $charSet])}]

    # Append the character at the random index to the password.
    append password [string index $charSet $index]
}

# Display the generated password.
puts "Your password is: $password"
```

**Explanation:**

* The `set` command is used to assign values to variables.
* The `puts` command is used to print text to the standard output.
* The `gets` command is used to read a line of text from the standard input.
* The `for` command is used to create a loop.
* The `expr` command is used to evaluate a mathematical expression.
* The `string index` command is used to extract a character from a string at a specified index.
* The `append` command is used to add a string to the end of another string.