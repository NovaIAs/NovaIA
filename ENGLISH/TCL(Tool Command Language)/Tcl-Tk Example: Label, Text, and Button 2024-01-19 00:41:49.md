```tcl
package require Tcl 8.5
package require Tk

# Create a new window
set w [tk toplevel]

# Set the window title
tk title $w "A Complex Tcl/Tk Example"

# Create a frame to hold all the controls
set f [tk frame $w]
tk pack $f -side top -anchor nw

# Create a label to display some text
set label [tk label $f -text "This is a complex Tcl/Tk example."]
tk pack $label -side top

# Create a text widget to display some text
set text [tk text $f -width 40 -height 10]
tk pack $text -side top -fill both

# Create a button to press
set button [tk button $f -text "Press me!"]
tk pack $button -side top

# Define a command to be executed when the button is pressed
proc buttonCallback { } {
    # Get the current text in the text widget
    set text [tk get $text]

    # Add a new line to the text widget
    tk insert $text end "\nThis is line $::num"

    # Increment the line number
    incr ::num
}

# Bind the button to the command
tk bind $button <Button-1> buttonCallback

# Keep the window open
tk mainloop
```

This code creates a Tcl/Tk window with a label, a text widget, and a button. When the button is pressed, the code appends a new line of text to the text widget. The text widget displays the line numbers of the lines of text that have been added.

Here is a breakdown of the code:

* The `package require` commands import the Tcl and Tk libraries.
* The `tk toplevel` command creates a new window.
* The `tk title` command sets the window title.
* The `tk frame` command creates a frame to hold the controls.
* The `tk label` command creates a label to display some text.
* The `tk text` command creates a text widget to display some text.
* The `tk button` command creates a button to press.
* The `proc` command defines a command to be executed when the button is pressed.
* The `tk bind` command binds the button to the command.
* The `tk mainloop` command keeps the window open.