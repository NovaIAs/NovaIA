```tcl
# Create a custom Tkinter window with a menu bar, toolbar, and status bar.

package require Tk

# Create the main window.
set main [toplevel .main]
wm title $main "Custom Tkinter Window"

# Create the menu bar.
set menubar [menu .main]
wm menubutton $main .menubar

# Create the "File" menu.
set filemenu [menu $menubar]
$filemenu add command -label "New" -command {fileNew}
$filemenu add command -label "Open..." -command {fileOpen}
$filemenu add command -label "Save" -command {fileSave}
$filemenu add command -label "Save As..." -command {fileSaveAs}
$filemenu add separator
$filemenu add command -label "Exit" -command [exit 0]

# Create the "Edit" menu.
set editmenu [menu $menubar]
$editmenu add command -label "Undo" -command {editUndo}
$editmenu add command -label "Redo" -command {editRedo}
$editmenu add separator
$editmenu add command -label "Cut" -command {editCut}
$editmenu add command -label "Copy" -command {editCopy}
$editmenu add command -label "Paste" -command {editPaste}

# Create the "Tools" menu.
set toolsmenu [menu $menubar]
$toolsmenu add command -label "Preferences" -command {toolsPreferences}

# Create the "Help" menu.
set helpmenu [menu $menubar]
$helpmenu add command -label "Help" -command {helpHelp}
$helpmenu add command -label "About" -command {helpAbout}

# Create the toolbar.
set toolbar [frame .main]
pack $toolbar -side top -fill x

# Create the "New" button.
set newButton [button $toolbar -text "New" -command {fileNew}]
pack $newButton -side left

# Create the "Open" button.
set openButton [button $toolbar -text "Open..." -command {fileOpen}]
pack $openButton -side left

# Create the "Save" button.
set saveButton [button $toolbar -text "Save" -command {fileSave}]
pack $saveButton -side left

# Create the status bar.
set statusbar [label .main -text "Ready" -relief sunk -bd 1]
pack $statusbar -side bottom -fill x

# Main event loop.
mainloop
```

This code creates a custom Tkinter window with a menu bar, toolbar, and status bar. The menu bar contains a "File" menu with options to create new files, open existing files, save files, and exit the program. The "Edit" menu contains options to undo and redo changes, as well as cut, copy, and paste text. The "Tools" menu contains an option to open the preferences dialog. The "Help" menu contains options to open the help documentation and the about dialog.

The toolbar contains buttons for creating new files, opening existing files, and saving files. The status bar displays the current status of the program.

The main event loop is started at the end of the script, which allows the program to respond to user input.