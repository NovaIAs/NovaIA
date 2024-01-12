```tcl
# This is a complex Tcl script that demonstrates the use of various Tcl features.

# Define a procedure to calculate the factorial of a number.
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Define a procedure to print a table of factorials.
proc print_factorial_table {n} {
    puts "Factorial Table:"
    for {set i 0} {$i <= $n} {incr i} {
        puts "[format "%2d! = %d" $i [factorial $i]]"
    }
}

# Define a procedure to generate a random number between 1 and 100.
proc random_number {} {
    return [expr {int(rand() * 100) + 1}]
}

# Define a procedure to play a simple guessing game.
proc play_guessing_game {} {
    # Generate a random number.
    set secret [random_number]

    # Get the user's guess.
    puts "Guess a number between 1 and 100:"
    set guess [gets stdin]

    # Check if the guess is correct.
    while {$guess != $secret} {
        if {$guess < $secret} {
            puts "Your guess is too low."
        } else {
            puts "Your guess is too high."
        }

        # Get the user's next guess.
        puts "Guess again:"
        set guess [gets stdin]
    }

    # The user guessed the correct number.
    puts "Congratulations! You guessed the correct number."
}

# Define a procedure to print a menu of options.
proc print_menu {} {
    puts "Menu:"
    puts "1. Print a table of factorials"
    puts "2. Play a guessing game"
    puts "3. Quit"
}

# Get the user's choice.
set choice [gets stdin]

# Execute the corresponding procedure based on the user's choice.
switch $choice {
    1 {
        print_factorial_table 10
    }
    2 {
        play_guessing_game
    }
    3 {
        exit
    }
    default {
        puts "Invalid choice."
    }
}
```

Explanation:

* The `factorial` procedure calculates the factorial of a number using recursion.
* The `print_factorial_table` procedure prints a table of factorials up to a specified number.
* The `random_number` procedure generates a random number between 1 and 100.
* The `play_guessing_game` procedure plays a simple guessing game with the user.
* The `print_menu` procedure prints a menu of options for the user to choose from.
* The main program gets the user's choice and executes the corresponding procedure.