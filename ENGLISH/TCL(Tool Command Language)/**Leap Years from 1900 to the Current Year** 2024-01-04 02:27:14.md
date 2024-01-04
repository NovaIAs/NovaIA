```tcl
# Initialize a variable to store the current year
set current_year [clock scan [clock format [clock seconds] -format "%Y"] %Y]

# Create a list of years from 1900 to the current year
set years [lrange [list 1900 $current_year] 0 end]

# Create an empty list to store the leap years
set leap_years {}

# Iterate through the list of years and check if each year is a leap year
foreach year $years {
    # Check if the year is divisible by 400 or by 4 but not by 100
    if {[expr {$year % 400 == 0} || ($year % 4 == 0 && $year % 100 != 0)]} {
        # If the year is a leap year, add it to the list of leap years
        lappend leap_years $year
    }
}

# Print the list of leap years
puts "Leap years from 1900 to $current_year:"
foreach year $leap_years {
    puts $year
}
```

Explanation:

1. Initialize a variable `current_year` to store the current year using the `clock` command.

2. Create a list of years from 1900 to the current year using the `lrange` command.

3. Create an empty list `leap_years` to store the leap years.

4. Iterate through the list of years using the `foreach` loop.

5. Inside the loop, check if the current year is a leap year using the `expr` command and the following conditions:
   - If the year is divisible by 400, it is a leap year.
   - If the year is divisible by 4 but not by 100, it is a leap year.

6. If the year is a leap year, add it to the `leap_years` list using the `lappend` command.

7. After iterating through all the years, print the list of leap years using the `puts` command.