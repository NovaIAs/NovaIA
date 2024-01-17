```tcl
# Define a list of colors
colors [list red orange yellow green blue indigo violet]

# Create a procedure to print a colored message
proc printMessage {color message} {
    # Set the console color
    set console [console]
    $console configure -background $color

    # Print the message
    puts [format "%s: %s" $color $message]

    # Reset the console color
    $console configure -background ""
}

# Create a procedure to generate a random number
proc randomNumber {min max} {
    return [expr {$min + (rand() * ($max + 1 - $min))}]
}

# Create a procedure to play a game
proc playGame {turns} {
    # Initialize the score
    set score 0

    # Iterate over the number of turns
    for {set i 0} {$i < $turns} {incr i} {
        # Generate a random color
        set color [lindex $colors [randomNumber 0 [llength $colors - 1]]]

        # Generate a random number
        set number [randomNumber 1 10]

        # Prompt the user to guess the color
        puts [format "Guess the color (%s) of the number %d: " $color $number]

        # Read the user's guess
        set guess [gets stdin]

        # Check if the guess is correct
        if {$guess == $color} {
            # Increment the score
            incr score

            # Print a success message
            printMessage green "Correct!"
        } else {
            # Print a failure message
            printMessage red "Incorrect!"
        }
    }

    # Print the final score
    puts [format "Your score is %d", $score]
}

# Play the game
playGame 10
```

Explanation:

* The code defines a list of colors, a procedure to print a colored message, a procedure to generate a random number, and a procedure to play a game.
* The `playGame` procedure initializes the score to 0 and then iterates over the number of turns.
* In each turn, it generates a random color and a random number, prompts the user to guess the color of the number, reads the user's guess, and checks if the guess is correct.
* If the guess is correct, the score is incremented and a success message is printed. Otherwise, a failure message is printed.
* After all the turns are played, the final score is printed.