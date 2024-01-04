```tcl
# Create a procedure to generate a random number between two values
proc generate_random_number {min max} {
    # Calculate the range of possible values
    set range [expr {$max - $min + 1}]

    # Generate a random number within the range
    set random [expr {$min + [rand $range]}]

    # Return the random number
    return $random
}

# Create a list of words
set words {apple banana cherry dog elephant fish grape house ice cream juice kiwi lemon mango nest orange pear queen rose strawberry tree umbrella violin watermelon xylophone yogurt zebra}

# Generate a random number of words to include in the poem
set num_words [generate_random_number 5 10]

# Create a variable to store the poem
set poem ""

# Loop through the desired number of words
for {set i 0} {$i < $num_words} {incr i} {
    # Generate a random index into the list of words
    set random_index [generate_random_number 0 [llength $words] - 1]

    # Get the word at the random index
    set word [lindex $words $random_index]

    # Add the word to the poem, followed by a space
    append poem "$word "
}

# Remove the trailing space from the poem
set poem [string trimright $poem]

# Print the poem
puts $poem
```

Explanation:

1. **Procedure to Generate a Random Number:**
   - We define a procedure called `generate_random_number` that takes two arguments: `min` and `max`.
   - It calculates the range of possible values (`range`) by subtracting `min` from `max` and adding 1.
   - It generates a random number within the range using the `rand` command and adds it to `min` to get the final random number.
   - The procedure returns the randomly generated number.

2. **List of Words:**
   - We create a list of words called `words` containing various words.

3. **Generate a Random Number of Words:**
   - We generate a random number between 5 and 10 using the `generate_random_number` procedure and store it in the variable `num_words`. This number determines how many words will be included in the poem.

4. **Create a Variable for the Poem:**
   - We initialize a variable called `poem` to store the generated poem.

5. **Loop to Generate the Poem:**
   - We use a `for` loop to iterate from 0 to `num_words - 1`.
   - Inside the loop, we generate a random index into the `words` list using the `generate_random_number` procedure.
   - We get the word at the random index and append it to the `poem` variable, followed by a space.

6. **Remove Trailing Space:**
   - After generating the poem, we use the `string trimright` command to remove any trailing space from the poem.

7. **Print the Poem:**
   - Finally, we use the `puts` command to print the generated poem to the console.

This code generates a random poem by randomly selecting words from a list and then assembling them into a poem of a specified length. The random nature of the word selection and the number of words included makes each poem unique and unpredictable.