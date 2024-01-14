```tcl
# This Tcl script demonstrates the use of various advanced features of the language, including procedures, list processing, and file manipulation.

# Define a procedure to calculate the factorial of a number
proc factorial {n} {
    if {$n == 0} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}

# Create a list of numbers from 1 to 10
numbers [lrange [seq 1 10]]

# Calculate the factorial of each number in the list using a foreach loop
foreach number $numbers {
    set factorial [factorial $number]
    puts "The factorial of $number is $factorial"
}

# Read the contents of a file line by line and print each line
set file [open "input.txt" r]
while {[gets $file line] != -1} {
    puts $line
}
close $file

# Create a new file and write some data to it
set file [open "output.txt" w]
puts $file "This is some data that I am writing to the file."
close $file

# Use the exec command to run an external command
exec ls -l

# Get the current working directory
set cwd [pwd]
puts "The current working directory is $cwd"

# Change the current working directory
cd /tmp
puts "The current working directory is now [pwd]"
```

This code demonstrates various advanced features of Tcl, including:

* **Procedures:** The `factorial` procedure is defined using the `proc` command. Procedures can be used to break down a complex task into smaller, more manageable steps.
* **List processing:** The `lrange` and `foreach` commands are used to process the list of numbers. Tcl has powerful list processing capabilities that make it easy to manipulate and transform data.
* **File manipulation:** The `open`, `gets`, `puts`, and `close` commands are used to read and write files. Tcl provides a simple and convenient way to interact with the file system.
* **External command execution:** The `exec` command is used to run an external command. This allows Tcl scripts to interact with other programs and system utilities.
* **Directory manipulation:** The `pwd` and `cd` commands are used to get and change the current working directory. Tcl provides a number of commands for working with directories and files.

This code is a comprehensive demonstration of the power and flexibility of Tcl. It shows how Tcl can be used to perform a variety of tasks, from simple calculations to complex file processing.