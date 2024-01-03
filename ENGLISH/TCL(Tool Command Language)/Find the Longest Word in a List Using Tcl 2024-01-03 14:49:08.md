```tcl
# Create a list of words
set words {apple orange banana grape pineapple strawberry}

# Create a procedure to find the longest word in a list
proc findLongestWord {list} {
    # Set the longest word to an empty string
    set longest ""

    # Loop through each word in the list
    foreach word $list {
        # If the word is longer than the longest word, update the longest word
        if {[string length $word] > [string length $longest]} {
            set longest $word
        }
    }

    # Return the longest word
    return $longest
}

# Find the longest word in the list of words
set longestWord [findLongestWord $words]

# Print the longest word
puts "The longest word is: $longestWord"
```

This code performs the following tasks:

1. It creates a list of words using the `set` command.
2. It defines a procedure called `findLongestWord` using the `proc` command. This procedure takes a list of words as input and returns the longest word in the list.
3. Inside the `findLongestWord` procedure, it initializes the `longest` variable to an empty string.
4. It then iterates through each word in the input list using the `foreach` command.
5. For each word, it checks if the length of the word is greater than the length of the current longest word. If it is, it updates the `longest` variable to the current word.
6. After processing all the words in the list, it returns the `longest` variable, which contains the longest word in the list.
7. Finally, it calls the `findLongestWord` procedure with the list of words and prints the result using the `puts` command.