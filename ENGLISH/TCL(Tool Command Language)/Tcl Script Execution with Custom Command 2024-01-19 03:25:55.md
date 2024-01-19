```tcl
# Creating a custom Tcl command
proc my_custom_command {args} {
    # Check if the correct number of arguments were provided
    if {[llength $args] != 3} {
        error "Usage: my_custom_command <argument1> <argument2> <argument3>"
    }

    # Extract the arguments
    set arg1 [lindex $args 0]
    set arg2 [lindex $args 1]
    set arg3 [lindex $args 2]

    # Perform some operations using the arguments
    set result [expr {$arg1 + $arg2 + $arg3}]

    # Return the result
    return $result
}

# Define a Tcl script to be executed on startup
package require Tcl 8.5
package require Tk

# Create a new window
toplevel .main

# Add a label to the window
label .main.label -text "Tcl Script Execution" -font Arial 12

# Create a text widget to display the result
text .main.result -width 40 -height 10

# Create a button to execute the custom command
button .main.execute -text "Execute Command" -command {
    set result [my_custom_command 1 2 3]
    .main.result insert end "The result is $result\n"
}

# Start the Tcl event loop
wm title .main "Tcl Script Execution"
wm geometry .main 300x200
.main.mainloop
```

Explanation:

1. `proc my_custom_command {args}`: This line defines a new Tcl command named `my_custom_command` that takes three arguments.

2. `if {[llength $args] != 3}`: This line checks if the number of arguments provided to the command is not equal to 3. If it is not, an error message is displayed.

3. `set arg1 [lindex $args 0]`: This line extracts the first argument from the list of arguments and assigns it to the variable `arg1`.

4. `set arg2 [lindex $args 1]`: This line extracts the second argument from the list of arguments and assigns it to the variable `arg2`.

5. `set arg3 [lindex $args 2]`: This line extracts the third argument from the list of arguments and assigns it to the variable `arg3`.

6. `set result [expr {$arg1 + $arg2 + $arg3}]`: This line performs the addition of the three arguments and stores the result in the variable `result`.

7. `return $result`: This line returns the value of the `result` variable as the result of the `my_custom_command`.

8. `package require Tcl 8.5`: This line ensures that the Tcl version 8.5 or higher is available.

9. `package require Tk`: This line loads the Tk library, which is a toolkit for creating graphical user interfaces in Tcl.

10. `toplevel .main`: This line creates a new top-level window with the name `.main`.

11. `label .main.label -text "Tcl Script Execution" -font Arial 12`: This line creates a label widget with the text "Tcl Script Execution" and sets its font to Arial with a size of 12.

12. `text .main.result -width 40 -height 10`: This line creates a text widget with a width of 40 characters and a height of 10 lines.

13. `button .main.execute -text "Execute Command" -command {...}`: This line creates a button with the text "Execute Command". When the button is clicked, the code inside the curly braces is executed.

14. In the button's command, the `my_custom_command` is called with the arguments 1, 2, and 3. The result of the command is stored in the variable `result`.

15. `text .main.result insert end "The result is $result\n"`: This line inserts the string "The result is $result" followed by a newline character into the text widget.

16. `wm title .main "Tcl Script Execution"`: This line sets the title of the main window to "Tcl Script Execution".

17. `wm geometry .main 300x200`: This line sets the geometry of the main window to a size of 300 pixels wide and 200 pixels high.

18. `.main.mainloop`: This line starts the Tcl event loop, which allows the graphical user interface to respond to user interactions.