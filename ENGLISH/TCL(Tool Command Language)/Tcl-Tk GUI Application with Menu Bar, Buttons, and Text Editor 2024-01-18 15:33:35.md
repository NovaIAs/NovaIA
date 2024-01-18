```tcl
package require Tcl 8.6
package require Tk 8.6

proc create_window {title} {
    toplevel .window$title
    wm title .window$title $title
    wm geometry .window$title 600x400
}

proc create_button {window text command} {
    button .window$window.$text -text $text -command $command
    pack .window$window.$text -side left -fill both -expand yes
}

proc open_file_dialog {initialdir} {
    filedialog .${initialdir} -mode open -title "Open File"
}

proc save_file_dialog {initialdir} {
    filedialog .${initialdir} -mode save -title "Save File"
}

proc create_menu {window} {
    menu .window$window.menubar
    menu .window$window.filemenu -tearoff 0
    menu .window$window.editmenu -tearoff 0

    menu .window$window.filemenu add command -label "New" -command "create_window New"
    menu .window$window.filemenu add command -label "Open" -command "open_file_dialog startdir"
    menu .window$window.filemenu add command -label "Save" -command "save_file_dialog startdir"
    menu .window$window.filemenu add command -label "Exit" -command "exit"

    menu .window$window.editmenu add command -label "Cut" -command "::tk::mac::cut .window$window.text"
    menu .window$window.editmenu add command -label "Copy" -command "::tk::mac::copy .window$window.text"
    menu .window$window.editmenu add command -label "Paste" -command "::tk::mac::paste .window$window.text"

    pack .window$window.menubar -side top -fill x
}

create_window Main
create_button Main New create_window "New Window"
create_button Main Open "open_file_dialog startdir"
create_button Main Save "save_file_dialog startdir"
create_menu Main

text .windowMain.text -yscrollcommand ".windowMain.yscrollbar set"
scrollbar .windowMain.yscrollbar -orient vertical -command ".windowMain.text yview"
pack .windowMain.text -side left -fill both -expand yes
pack .windowMain.yscrollbar -side right -fill y

wm deiconify .
```

Explanation:

This Tcl/Tk script creates a simple GUI application with a main window, a menu bar, and a text editor.

1. **Package Require:** The first few lines load the necessary Tcl and Tk packages.

2. **Window Creation:** The `create_window` procedure is defined to create a new window with a given title.

3. **Button Creation:** The `create_button` procedure is defined to create a new button in a specified window.

4. **File Dialogs:** The `open_file_dialog` and `save_file_dialog` procedures are defined to open a file dialog for opening or saving a file, respectively.

5. **Menu Creation:** The `create_menu` procedure is defined to create a menu bar with a "File" and "Edit" menu.

6. **GUI Initialization:**
   - A main window named "Main" is created using `create_window`.
   - Three buttons are created in the main window using `create_button`.
   - A text editor widget is created and packed in the main window.
   - A menu bar is created and packed in the main window using `create_menu`.

7. **Event Handling:** Scrollbar commands are set for the text editor to enable vertical scrolling.

8. **Application Launch:** The `wm deiconify` command is used to display the main window.

This code provides a basic framework for creating a GUI application in Tcl/Tk. It can be customized and expanded to create more complex applications.