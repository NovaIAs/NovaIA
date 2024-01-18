```tcl
# Define a procedure to generate a random number between two limits.
proc randRange {min max} {
    expr {int(rand() * ($max - $min + 1)) + $min}
}

# Define a procedure to generate a random string of a given length.
proc randString {length} {
    set chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    set result ""
    for {set i 0} {$i < $length} {incr i} {
        append result [string index $chars [randRange 0 [llength $chars] - 1]]
    }
    return $result
}

# Define a procedure to generate a random password.
proc generatePassword {length} {
    set result ""
    for {set i 0} {$i < $length} {incr i} {
        append result [randString 1]
    }
    return $result
}

# Generate a random password of length 10.
set password [generatePassword 10]

# Print the generated password to the console.
puts "Generated password: $password"

```

This code is a Tcl script that generates a random password of a given length. It uses the `randRange` procedure to generate a random number between two limits, the `randString` procedure to generate a random string of a given length, and the `generatePassword` procedure to generate a random password. The `generatePassword` procedure calls the `randString` procedure to generate a random string of the specified length and then concatenates these strings together to form the password. The `puts` command is used to print the generated password to the console.