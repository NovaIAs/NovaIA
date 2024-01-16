```tcl
# Complex TCL script with multiple functionalities

# Define a procedure to calculate the factorial of a number
proc factorial {number} {
    if {$number == 0} {
        return 1
    } else {
        return [expr {$number * [factorial [expr {$number - 1}]]}]
    }
}

# Define a procedure to check if a number is prime
proc isPrime {number} {
    if {$number <= 1} {
        return 0
    }
    for {set i 2} {$i * $i <= $number} {incr i} {
        if {[expr {$number % $i}] == 0} {
            return 0
        }
    }
    return 1
}

# Define a procedure to generate a list of prime numbers up to a given limit
proc generatePrimes {limit} {
    set primes {}
    for {set i 2} {$i <= $limit} {incr i} {
        if {[isPrime $i]} {
            lappend primes $i
        }
    }
    return $primes
}

# Define a procedure to calculate the greatest common divisor of two numbers
proc gcd {a b} {
    if {$b == 0} {
        return $a
    } else {
        return [gcd $b [expr {$a % $b}]]
    }
}

# Define a procedure to calculate the least common multiple of two numbers
proc lcm {a b} {
    return [expr {$a * $b / [gcd $a $b]}]
}

# Define a procedure to find the roots of a quadratic equation
proc quadraticRoots {a b c} {
    set discriminant [expr {$b * $b - 4 * $a * $c}]
    if {$discriminant < 0} {
        return "No real roots"
    } elseif {$discriminant == 0} {
        return [expr {-b / (2 * $a)}]
    } else {
        set sqrt_discriminant [expr {sqrt($discriminant)}]
        return [expr {(-b + $sqrt_discriminant) / (2 * $a)}, expr {(-b - $sqrt_discriminant) / (2 * $a)}]
    }
}

# Define a procedure to generate a random number between two values
proc random {min max} {
    return [expr {int(rand() * ($max - $min + 1)) + $min}]
}

# Define a procedure to convert a temperature from Fahrenheit to Celsius
proc fahrenheitToCelsius {fahrenheit} {
    return [expr {($fahrenheit - 32) * 5 / 9}]
}

# Define a procedure to convert a temperature from Celsius to Fahrenheit
proc celsiusToFahrenheit {celsius} {
    return [expr {($celsius * 9 / 5) + 32}]
}

# Define a procedure to convert a distance from miles to kilometers
proc milesToKilometers {miles} {
    return [expr {$miles * 1.60934}]
}

# Define a procedure to convert a distance from kilometers to miles
proc kilometersToMiles {kilometers} {
    return [expr {$kilometers / 1.60934}]
}

# Define a procedure to generate a random string of a given length
proc randomString {length} {
    set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    set random_string ""
    for {set i 0} {$i < $length} {incr i} {
        set random_string [concat $random_string [string range $characters [random 0 [string length $characters - 1]] 1]]
    }
    return $random_string
}

# Define a procedure to get the current date and time
proc getCurrentDateTime {} {
    return [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
}

# Define a procedure to sleep for a given number of seconds
proc sleep {seconds} {
    after $seconds {}
}

# Define a procedure to execute a system command
proc systemCommand {command} {
    set output [exec $command]
    return $output
}

# Define a procedure to create a new file
proc createFile {filename} {
    file open $filename w
    file close $filename
}

# Define a procedure to write data to a file
proc writeToFile {filename data} {
    file open $filename a
    file puts $filename $data
    file close $filename
}

# Define a procedure to read data from a file
proc readFromFile {filename} {
    set data [file read $filename]
    return $data
}

# Define a procedure to delete a file
proc deleteFile {filename} {
    file delete $filename
}

# Define a procedure to create a new directory
proc createDirectory {directory} {
    file mkdir $directory
}

# Define a procedure to delete a directory
proc deleteDirectory {directory} {
    file rmdir $directory
}

# Define a procedure to list the files in a directory
proc listFiles {directory} {
    return [glob -directory $directory *]
}

# Define a procedure to copy a file
proc copyFile {source destination} {
    file copy $source $destination
}

# Define a procedure to move a file
proc moveFile {source destination} {
    file rename $source $destination
}

# Define a procedure to get the file extension of a file
proc getFileExtension {filename} {
    set parts [split $filename .]
    return $parts[end]
}

# Define a procedure to get the file size of a file
proc getFileSize {filename} {
    return [file size $filename]
}

# Define a procedure to get the last modified time of a file
proc getLastModifiedTime {filename} {
    return [file mtime $filename]
}

# Define a procedure to get the file permissions of a file
proc getFilePermissions {filename} {
    return [file atime $filename]
}

# Define a procedure to set the file permissions of a file
proc setFilePermissions {filename permissions} {
    file atime $filename $permissions
}

# Define a procedure to create a new process
proc createProcess {command} {
    return [exec $command &]
}

# Define a procedure to kill a process
proc killProcess {pid} {
    exec kill $pid
}

# Define a procedure to send a signal to a process
proc sendSignal {pid signal} {
    exec kill -s $signal $pid
}

# Define a procedure to get the current process ID
proc getCurrentProcessID {} {
    return [pid]
}

# Define a procedure to get the parent process ID
proc getParentProcessID {} {
    return [set ppid]
}

# Define a procedure to get the current user ID
proc getCurrentUserID {} {
    return [uid]
}

# Define a procedure to get the current group ID
proc getCurrentGroupID {} {
    return [gid]
}

# Define a procedure to get the current working directory
proc getCurrentWorkingDirectory {} {
    return [pwd]
}

# Define a procedure to change the current working directory
proc changeWorkingDirectory {directory} {
    cd $directory
}

# Define a procedure to get the environment variables
proc getEnvironmentVariables {} {
    return [env]
}
```