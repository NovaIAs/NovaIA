```tcl
# START OF CODE

# Import necessary libraries
package require Tcl 8.5
package require Tk 8.5

# Define the main window
set mainWin [tk toplevel]
tk title $mainWin "Main Window"

# Create a text editor widget
set textEditor [tk text $mainWin -wrap word]
tk grid $textEditor -sticky nsew

# Create a toolbar with buttons
set toolbar [tk frame $mainWin]
tk grid $toolbar -sticky ew

# Define the toolbar buttons
set buttons {
    new [tk button $toolbar -text "New"]
    open [tk button $toolbar -text "Open"]
    save [tk button $toolbar -text "Save"]
    quit [tk button $toolbar -text "Quit"]
}

# Set the button commands
foreach button [lsort [dict keys $buttons]] {
    tk config $buttons($button) -command "::tclCode \$button"
}

# Pack the buttons in the toolbar
foreach button [lsort [dict keys $buttons]] {
    tk pack $buttons($button) -side left
}

# Function to create a new file
proc new {} {
    puts "Creating a new file."
    $textEditor delete 1.0 end
}

# Function to open a file
proc open {} {
    puts "Opening a file."
    set filename [tk get open]
    $textEditor delete 1.0 end
    $textEditor insert 1.0 [read $filename]
}

# Function to save a file
proc save {} {
    puts "Saving a file."
    set filename [tk get save]
    write $filename [$textEditor get 1.0 end]
}

# Function to quit the application
proc quit {} {
    puts "Quitting the application."
    exit
}

# Set the default file types
set defaultTypes {
    *.txt Text files
    *.py Python scripts
    *.tcl Tcl scripts
}

# Create the file open and save dialogs
set openDialog [tk filedialog $mainWin -title "Open File" -defaultextension .txt -filetypes $defaultTypes]
set saveDialog [tk filedialog $mainWin -title "Save File" -defaultextension .txt -filetypes $defaultTypes]

# Bind the buttons to the dialogs
tk bind $buttons(open) <Button-1> {tk setvar open $openDialog}
tk bind $buttons(save) <Button-1> {tk setvar save $saveDialog}

# Main event loop
tk main

# END OF CODE
```

This code creates a simple text editor application in TCL (Tool Command Language). It has a main window with a text editor widget, a toolbar with buttons for new, open, save, and quit, and file open and save dialogs. The code is well-commented and easy to understand.

Here is a detailed explanation of the code:

1. Import Libraries:
   - `package require Tcl 8.5`: Imports the Tcl library version 8.5.
   - `package require Tk 8.5`: Imports the Tk library version 8.5, which provides the graphical user interface components.

2. Define the Main Window:
   - `set mainWin [tk toplevel]`: Creates the main application window.
   - `tk title $mainWin "Main Window"`: Sets the title of the main window to "Main Window."

3. Create Text Editor Widget:
   - `set textEditor [tk text $mainWin -wrap word]`: Creates a text editor widget inside the main window. The `-wrap word` option ensures that words don't get broken across lines.
   - `tk grid $textEditor -sticky nsew`: Adds the text editor to the main window and makes it resizable in all directions.

4. Create Toolbar:
   - `set toolbar [tk frame $mainWin]`: Creates a frame to hold the toolbar.
   - `tk grid $toolbar -sticky ew`: Adds the toolbar to the main window and makes it resizable horizontally.

5. Define Toolbar Buttons:
   - `set buttons { ... }`: Defines a list of buttons with their corresponding labels.

6. Set Button Commands:
   - The `foreach` loop iterates over the button labels, sets their commands to call specific functions when clicked, and packs them in the toolbar.

7. Define Button Functions:
   - `proc new {}`: Function to create a new file.
   - `proc open {}`: Function to open a file.
   - `proc save {}`: Function to save a file.
   - `proc quit {}`: Function to quit the application.

8. Set Default File Types:
   - `set defaultTypes { ... }`: Defines the default file types and their descriptions for the file open and save dialogs.

9. Create File Open and Save Dialogs:
   - `set openDialog [ ... ]`: Creates a file open dialog.
   - `set saveDialog [ ... ]`: Creates a file save dialog.

10. Bind Buttons to Dialogs:
    - `tk bind $buttons(open) <Button-1> { ... }`: Binds the open button to the file open dialog.
    - `tk bind $buttons(save) <Button-1> { ... }`: Binds the save button to the file save dialog.

11. Main Event Loop:
    - `tk main`: Starts the main event loop, which listens for user interactions and updates the GUI accordingly.