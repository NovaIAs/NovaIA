```tcl
proc generate_random_string {length} {
    set chars {abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789}
    set result {}
    for {set i 0} {$i < $length} {incr i} {
        set result [concat $result [string range $chars [rand $length]]]
    }
    return $result
}

proc generate_random_numbers {length} {
    set result {}
    for {set i 0} {$i < $length} {incr i} {
        set result [concat $result [rand 100]]
    }
    return $result
}

proc generate_random_dates {length} {
    set result {}
    for {set i 0} {$i < $length} {incr i} {
        set day [rand 31]
        set month [rand 12]
        set year [rand 2023]
        set result [concat $result [format "%02d/%02d/%04d" $day $month $year]]
    }
    return $result
}

proc generate_random_data {type length} {
    switch -exact $type {
        string {
            return [generate_random_string $length]
        }
        number {
            return [generate_random_numbers $length]
        }
        date {
            return [generate_random_dates $length]
        }
        default {
            error "Invalid type: $type"
        }
    }
}

proc generate_random_dataset {size types} {
    set dataset {}
    for {set i 0} {$i < $size} {incr i} {
        set type [lindex $types [rand [llength $types]]]
        set data [generate_random_data $type 10]
        set dataset [lset dataset $i $data]
    }
    return $dataset
}

proc print_dataset {dataset} {
    puts "Random Dataset:"
    foreach data $dataset {
        puts $data
    }
}

# Generate a random dataset of 100 entries, with three types of data: string, number, and date
set dataset [generate_random_dataset 100 {string number date}]

# Print the dataset to the console
print_dataset $dataset
```

This code generates a random dataset of a specified size, with a specified mix of data types. The data types can be string, number, or date. The code uses the `generate_random_data` procedure to generate random data of a specified type and length. The `generate_random_dataset` procedure uses the `generate_random_data` procedure to generate a dataset of a specified size, with a specified mix of data types. The `print_dataset` procedure prints the dataset to the console.

To use the code, you can copy and paste it into a Tcl interpreter, or you can save it in a file and source the file into the interpreter. For example, to save the code in a file named `generate_random_dataset.tcl`, you would use the following command:

```
save generate_random_dataset.tcl
```

To source the file into the interpreter, you would use the following command:

```
source generate_random_dataset.tcl
```

Once the code is loaded into the interpreter, you can use it to generate a random dataset by calling the `generate_random_dataset` procedure. For example, to generate a dataset of 100 entries, with three types of data: string, number, and date, you would use the following command:

```
set dataset [generate_random_dataset 100 {string number date}]
```

You can then print the dataset to the console using the `print_dataset` procedure. For example, to print the dataset to the console, you would use the following command:

```
print_dataset $dataset
```

The output of the code will be a random dataset of 100 entries, with three types of data: string, number, and date. The dataset will be printed to the console in the following format:

```
Random Dataset:
string1
number1
date1
string2
number2
date2
...
```