```tcl
# Define a custom Tcl command called "generateStrings"
proc generateStrings {numStrings length} {
    # Create an empty list to store the generated strings
    set strings {}

    # Loop through the specified number of strings
    for {set i 0} {$i < $numStrings} {incr i} {
        # Generate a random string of the specified length
        set string [generateRandomString $length]

        # Add the generated string to the list
        lappend strings $string
    }

    # Return the list of generated strings
    return $strings
}

# Define a custom Tcl command called "generateRandomString"
proc generateRandomString {length} {
    # Create an empty string to store the random string
    set string ""

    # Loop through the specified length
    for {set i 0} {$i < $length} {incr i} {
        # Generate a random character between 'a' and 'z'
        set char [string range [expr {int(rand() * 26)}] 1]

        # Append the random character to the string
        append string $char
    }

    # Return the generated string
    return $string
}

# Define a custom Tcl command called "findPalindromes"
proc findPalindromes {list} {
    # Create an empty list to store the palindromes
    set palindromes {}

    # Loop through each string in the list
    foreach string $list {
        # Check if the string is a palindrome
        if {[string compare $string [string reverse $string]] eq 0} {
            # Add the palindrome to the list
            lappend palindromes $string
        }
    }

    # Return the list of palindromes
    return $palindromes
}

# Define a custom Tcl command called "printResults"
proc printResults {strings palindromes} {
    # Print the generated strings
    puts "Generated Strings:"
    foreach string $strings {
        puts $string
    }

    # Print the palindromes
    puts "\nPalindromes:"
    foreach palindrome $palindromes {
        puts $palindrome
    }
}

# Generate 100 random strings of length 10
set strings [generateStrings 100 10]

# Find the palindromes among the generated strings
set palindromes [findPalindromes $strings]

# Print the results
printResults $strings $palindromes
```

Explanation:

1. We define custom Tcl commands called "generateStrings", "generateRandomString", "findPalindromes", and "printResults". These commands perform specific tasks related to generating random strings, finding palindromes, and printing the results.

2. In the "generateStrings" command, we define a loop to generate a specified number of random strings of a specified length. We call the "generateRandomString" command within the loop to generate each random string.

3. In the "generateRandomString" command, we define another loop to generate a random string of the specified length. We generate random characters and append them to a string until the desired length is reached.

4. In the "findPalindromes" command, we define a loop to iterate through a list of strings. Within the loop, we check if each string is a palindrome by comparing it to its reversed version. If a string is a palindrome, we add it to a list of palindromes.

5. In the "printResults" command, we define a loop to print the generated strings and a loop to print the palindromes. This command displays the results in a clear and organized manner.

6. We generate 100 random strings of length 10 using the "generateStrings" command and store them in the "strings" variable.

7. We find the palindromes among the generated strings using the "findPalindromes" command and store them in the "palindromes" variable.

8. Finally, we use the "printResults" command to display the generated strings and the palindromes in a user-friendly format.

This code demonstrates the use of custom Tcl commands, loops, and conditional statements to generate random strings, find palindromes, and display the results.