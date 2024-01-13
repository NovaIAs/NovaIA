```tcl
# Create a new Tcl interpreter and assign it to a variable
interp = Tcl_CreateInterp()

# Load the Tcl script into the interpreter
Tcl_EvalFile(interp, "script.tcl")

# Create a new variable in the interpreter and set its value to "Hello, world!"
Tcl_SetVar(interp, "greeting", "Hello, world!")

# Evaluate a Tcl command in the interpreter
result = Tcl_Eval(interp, "puts $greeting")

# Print the result of the command
puts $result

# Delete the interpreter
Tcl_DeleteInterp(interp)

```

Explanation:

1. `interp = Tcl_CreateInterp()`: This line creates a new Tcl interpreter and assigns it to the variable `interp`. An interpreter is an environment in which Tcl scripts can be executed.

2. `Tcl_EvalFile(interp, "script.tcl")`: This line loads the Tcl script from the file `script.tcl` into the interpreter. The script is executed immediately, and any commands or functions defined in the script become available for use in the interpreter.

3. `Tcl_SetVar(interp, "greeting", "Hello, world!")`: This line creates a new variable called `greeting` in the interpreter and sets its value to `"Hello, world!"`. This means that the variable `greeting` can now be used in Tcl commands to access the string `"Hello, world!"`.

4. `result = Tcl_Eval(interp, "puts $greeting")`: This line evaluates the Tcl command `puts $greeting` in the interpreter. The `puts` command is used to print a value to the standard output. In this case, it prints the value of the variable `greeting`, which is `"Hello, world!"`. The result of the command is stored in the variable `result`.

5. `puts $result`: This line prints the value of the variable `result` to the standard output. Since `result` contains the string `"Hello, world!"`, this line will print `"Hello, world!"` to the console.

6. `Tcl_DeleteInterp(interp)`: This line deletes the interpreter that was created earlier. This frees up the memory that was allocated for the interpreter and its associated data structures.