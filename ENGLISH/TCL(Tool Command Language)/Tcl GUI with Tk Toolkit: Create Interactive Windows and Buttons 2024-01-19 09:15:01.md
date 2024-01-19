```tcl
# Start by defining some helper procedures

proc create_window {title width height} {
    toplevel .new_window
    wm title .new_window $title
    wm geometry .new_window $width x $height
}

proc create_button {window text command} {
    button .new_button -text $text -command $command
    pack .new_button -side left
}

proc create_label {window text} {
    label .new_label -text $text
    pack .new_label -side left
}

# Now, let's create a main window

create_window "Main Window" 400 300

# Add a button to the main window

create_button .new_window "Click Me!" {
    create_window "New Window" 200 100
    create_label .new_window "Hello, world!"
}

# Enter the event loop

mainloop
```

This code creates a simple graphical user interface (GUI) using the Tk toolkit, which is a standard library for creating GUIs in Tcl. The code first defines three helper procedures:

* `create_window`: This procedure creates a new window with the specified title, width, and height.
* `create_button`: This procedure creates a new button with the specified text and command.
* `create_label`: This procedure creates a new label with the specified text.

Next, the code creates a main window using the `create_window` procedure. The main window has the title "Main Window" and is 400 pixels wide by 300 pixels high.

The code then adds a button to the main window using the `create_button` procedure. The button has the text "Click Me!" and, when clicked, will call the `create_window` procedure to create a new window.

Finally, the code enters the event loop using the `mainloop` procedure. The event loop waits for events to occur, such as mouse clicks and key presses, and then calls the appropriate event handlers.

This code is a bit more complex than the previous examples, but it shows how Tcl can be used to create more sophisticated GUIs.