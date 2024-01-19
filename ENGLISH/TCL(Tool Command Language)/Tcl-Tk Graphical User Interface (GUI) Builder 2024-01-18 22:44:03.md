```tcl
# Define a macro to create a new window
macro createWindow {title width height} {
    # Create a new toplevel window
    toplevel .window$title
    # Set the window title
    wm title .window$title $title
    # Set the window size
    wm geometry .window$title $width x $height
    # Center the window on the screen
    wm deiconify .window$title
    wm center .window$title
    # Return the handle to the new window
    return .window$title
}

# Create a new window with the title "Main Window"
set mainWindow [createWindow "Main Window" 800 600]

# Create a new frame inside the main window
frame .mainWindow.frame
# Set the frame's background color
.mainWindow.frame configure -background #ffffff
# Pack the frame into the main window
pack .mainWindow.frame -expand yes -fill both

# Create a new label inside the frame
label .mainWindow.frame.label \
    -text "This is a label" \
    -background #ffffff
# Pack the label into the frame
pack .mainWindow.frame.label -side top

# Create a new button inside the frame
button .mainWindow.frame.button \
    -text "This is a button" \
    -command {puts "You clicked the button!"}
# Pack the button into the frame
pack .mainWindow.frame.button -side bottom

# Create a new menubar for the main window
menubar .mainWindow.menubar
# Add a menu to the menubar
menu .mainWindow.menubar.file \
    -label "File"
# Add a menu item to the File menu
menu .mainWindow.menubar.file.open \
    -label "Open" \
    -command {puts "You clicked Open"}
# Add a menu item to the File menu
menu .mainWindow.menubar.file.quit \
    -label "Quit" \
    -command {exit}
# Pack the menubar into the main window
pack .mainWindow.menubar -side top

# Start the main event loop
mainloop
```

This code creates a simple graphical user interface (GUI) in Tcl/Tk. The GUI consists of a main window with a title bar, a menubar, a frame, a label, and a button. The code is well-commented and easy to understand. Here is a breakdown of the code:

* The `createWindow` macro is defined to create a new window. This macro takes three arguments: the title of the window, the width of the window, and the height of the window. The macro creates a new toplevel window, sets the window title, sets the window size, centers the window on the screen, and returns the handle to the new window.
* The main window is created using the `createWindow` macro. The main window has the title "Main Window" and is 800 pixels wide and 600 pixels high.
* A new frame is created inside the main window using the `frame` command. The frame's background color is set to white. The frame is packed into the main window using the `pack` command.
* A new label is created inside the frame using the `label` command. The label's text is set to "This is a label" and its background color is set to white. The label is packed into the frame using the `pack` command.
* A new button is created inside the frame using the `button` command. The button's text is set to "This is a button" and its command is set to a Tcl script that prints "You clicked the button!" to the console. The button is packed into the frame using the `pack` command.
* A new menubar is created for the main window using the `menubar` command. A File menu is added to the menubar using the `menu` command. An Open menu item is added to the File menu using the `menu` command. A Quit menu item is added to the File menu using the `menu` command. The menubar is packed into the main window using the `pack` command.
* The main event loop is started using the `mainloop` command. This starts the GUI and allows the user to interact with it.

This code demonstrates how to create a simple GUI in Tcl/Tk. The code is well-commented and easy to understand, making it a good example for beginners who want to learn Tcl/Tk.