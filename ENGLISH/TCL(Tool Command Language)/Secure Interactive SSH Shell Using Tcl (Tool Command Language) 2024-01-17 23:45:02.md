```tcl
package require Tcl 8.5
package require Expect

# Prompt for Password
proc get_password {hostname username} {
    # Construct the Expect script command
    set expect_cmd "expect -re {\\$ |Password:}"

    # Send user & password, then the \r (Enter) character
    puts "Connecting to $hostname for user $username"
    puts -nonewline [expect -timeout 600 -command {
        set ret [send_user "$username\r"]
        if {$ret == -1} {
            return -1
        }
        sleep 0.5
        send_user "$password\r"
    } $expect_cmd]

    # Read the prompt to ensure we're connected
    return [expect -timeout 600 -re {\\$ |>} \
        -command { exp_continue }]
}

# Main Script

puts "Enter hostname:"
set hostname [gets stdin]

puts "Enter username:"
set username [gets stdin]

puts "Enter password:"
set password [gets -stdin]

if {[get_password $hostname $username] != 0} {
    puts "Login failed."
    exit 1
}

# Define Commands for Interactive Session

proc whoami {} {
    puts -nonewline [send    "whoami\r"]
}

proc lsfiles {} {
    puts -nonewline [send    "ls -l\r"]
}

proc runcommand {command} {
    send    "$command\r"
    expect -timeout 600 -re {\(\\$ |# |>} \
        -command { exp_continue }
}

while {1} {
    puts -nonewline "[1;31m[$username@$hostname]$[0m "
    flush stdout
    set cmd [gets stdin]
    if {[string compare $cmd "exit"] == 0} {
        break
    }
    switch $cmd {
        "whoami" {
            whoami
        }
        "lsfiles" {
            lsfiles
        }
        default {
            runcommand $cmd
        }
    }
}

puts ""
puts "Disconnected from $hostname."
```
Explanation:

1. **Package Loading:**
   - We load the `Tcl` and `Expect` packages, which are required for the functionality of the script.


2. **Password Prompting:**
   - The `get_password` procedure takes two arguments, `hostname` and `username`, and prompts the user to enter their password. It uses the `Expect` package to send commands and interact with the SSH server.


3. **Main Script:**
   - The script starts by prompting the user for the hostname, username, and password of the remote server.


4. **Connection Check:**
   - It then calls the `get_password` procedure to establish a connection and checks if the login is successful.


5. **Defining Commands:**
   - After establishing the connection, the script defines several procedures that can be used to execute commands on the remote server and display the results.


6. **Interactive Session:**
   - The script enters an interactive session, where the user can input commands to be executed on the server. Supported commands include `whoami` for displaying the current user, `lsfiles` for listing files in the current directory, and `runcommand` for executing any arbitrary command.


7. **Exiting the Session:**
   - The session continues until the user enters the `exit` command, at which point the script closes the SSH connection and terminates.


8. **Displaying Results:**
   - The results of each command executed on the server are displayed in the interactive session, providing a way for the user to interact with the remote system.


9. **Cleanup:**
   - When the session is terminated, the script prints a disconnection message and exits gracefully.