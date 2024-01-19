```tcl
package require Tcl 8.6
package require Tk 8.6

set var1 "Hello World!"
set var2 12345
set var3 [list 1 2 3 4 5]
set var4 [dict set v1 "Value 1" v2 "Value 2"]

proc my_proc {arg1 arg2} {
    puts "arg1 is $arg1"
    puts "arg2 is $arg2"
}

my_proc $var1 $var2

for {set i 0} {$i < llength $var3} {incr i} {
    puts [lindex $var3 $i]
}

foreach key value [dict keys $var4] {
    puts "Key: $key, Value: $value"
}

grid [button .b -text "Click Me!" -command {puts "Button clicked!"}]

mainloop
```

Explanation:

1. **Package Loading**: It loads the required Tcl and Tk packages.

2. **Variable Declarations**: Variables like `var1`, `var2`, `var3`, and `var4` are created with different values.

3. **Procedure Definition**: A procedure `my_proc` is defined to take two arguments and display them.

4. **Procedure Call**: `my_proc` is called with `$var1` and `$var2` as arguments.

5. **Looping through a List**: A `for` loop is used to iterate through the list `$var3` and print each element.

6. **Looping through a Dictionary**: A `foreach` loop is used to iterate through the keys and values of the dictionary `$var4` and print them.

7. **Creating a Tkinter Button**: A Tkinter button is created with the text "Click Me!" and a command to display a message when clicked.

8. **Main Event Loop**: The `mainloop` function is called to start the Tkinter event loop, which listens for user interactions.