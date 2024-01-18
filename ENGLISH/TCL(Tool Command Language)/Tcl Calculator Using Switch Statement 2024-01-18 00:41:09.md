```tcl
# Create a simple calculator using a switch statement

# Define the switch statement
switch -exact -glob {OPERATION} {
    "+" {
        # Addition operation
        set result [expr {$ARG1 + $ARG2}]
        puts "The result of $ARG1 + $ARG2 is $result"
    }
    "-" {
        # Subtraction operation
        set result [expr {$ARG1 - $ARG2}]
        puts "The result of $ARG1 - $ARG2 is $result"
    }
    "*" {
        # Multiplication operation
        set result [expr {$ARG1 * $ARG2}]
        puts "The result of $ARG1 * $ARG2 is $result"
    }
    "/" {
        # Division operation (with error checking)
        if {$ARG2 == 0} {
            puts "Error: Division by zero"
        } else {
            set result [expr {$ARG1 / $ARG2}]
            puts "The result of $ARG1 / $ARG2 is $result"
        }
    }
    default {
        # Default case (invalid operation)
        puts "Invalid operation: $OPERATION"
    }
}

# Get the operation and operands from the user
puts "Enter the operation (+, -, *, /): "
set OPERATION [gets stdin]

puts "Enter the first operand: "
set ARG1 [gets stdin]

puts "Enter the second operand: "
set ARG2 [gets stdin]

# Call the switch statement to perform the operation
switch -exact -glob {OPERATION} $OPERATION $ARG1 $ARG2

```

This Tcl script creates a simple calculator using a switch statement. The user is prompted to enter the operation (addition, subtraction, multiplication, or division) and the two operands. The script then uses a switch statement to perform the specified operation and display the result.

Here's a breakdown of the code:

1. Define the Switch Statement:
   ```tcl
   switch -exact -glob {OPERATION} {
       ...
   }
   ```
   - `switch`: This command introduces a switch statement in Tcl.
   - `-exact`: This option ensures that the pattern matching is exact and not case-insensitive.
   - `-glob`: This option allows us to use glob patterns in the case labels.
   - `{OPERATION}`: This is the variable that will be used to match the operation entered by the user.

2. Define Case Labels:
   ```tcl
   "+" {
       ...
   }
   "-" {
       ...
   }
   ...
   ```
   - Each case label represents an operation.
   - The `{...}` block contains the code to be executed when the corresponding case is matched.

3. Perform the Operation:
   Inside each case label, the appropriate operation is performed using the `expr` command. For example, in the addition case, `set result [expr {$ARG1 + $ARG2}]` calculates the sum of `$ARG1` and `$ARG2`.

4. Display the Result:
   After performing the operation, the result is displayed using the `puts` command.

5. Get User Input:
   The `gets stdin` command is used to read input from the user. It prompts the user to enter the operation and operands, and stores their responses in the `OPERATION`, `ARG1`, and `ARG2` variables, respectively.

6. Call the Switch Statement:
   Once the user has entered all the required information, the `switch` statement is called with the `OPERATION`, `ARG1`, and `ARG2` variables as arguments. This causes the script to execute the appropriate case label and perform the specified operation.

This script provides a simple and interactive way to perform basic arithmetic operations using Tcl. It demonstrates the use of switch statements, variables, and user input in Tcl.