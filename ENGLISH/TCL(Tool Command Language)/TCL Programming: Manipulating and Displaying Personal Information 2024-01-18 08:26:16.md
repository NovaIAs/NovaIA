```tcl
# Create a data structure to store information about each person
set personInfo {
    "John Smith" {
        age 30
        occupation "Software Engineer"
        hobbies "Programming, hiking, reading"
    }
    "Jane Doe" {
        age 25
        occupation "Teacher"
        hobbies "Reading, writing, traveling"
    }
    "Michael Jones" {
        age 40
        occupation "Doctor"
        hobbies "Golf, tennis, skiing"
    }
}

# Create a procedure to print information about a person
proc printPersonInfo {name} {
    # Check if the person exists in the data structure
    if {[info exists personInfo($name)]} {
        # Get the person's information
        set info $personInfo($name)

        # Print the person's name
        puts "Name: $name"

        # Print the person's age
        puts "Age: $info(age)"

        # Print the person's occupation
        puts "Occupation: $info(occupation)"

        # Print the person's hobbies
        puts "Hobbies: $info(hobbies)"
    } else {
        # The person does not exist
        puts "Person not found."
    }
}

# Print information about each person
foreach person [array names personInfo] {
    printPersonInfo $person
    puts ""
}

# Create a procedure to add a new person to the data structure
proc addPerson {name age occupation hobbies} {
    # Check if the person already exists in the data structure
    if {[info exists personInfo($name)]} {
        # The person already exists
        puts "Person already exists."
    } else {
        # Create a new entry for the person in the data structure
        set personInfo($name) {
            age $age
            occupation $occupation
            hobbies $hobbies
        }

        # Print a message to indicate that the person was added
        puts "Person added successfully."
    }
}

# Add a new person to the data structure
addPerson "Mary Johnson" 35 " Nurse" " Running, biking, cooking"

# Print information about the new person
printPersonInfo "Mary Johnson"
```

This code uses a data structure called an "array" to store information about each person. The array is indexed by the person's name, and each entry in the array contains the person's age, occupation, and hobbies.

The code also defines two procedures:

* `printPersonInfo`: This procedure prints information about a person, given their name.
* `addPerson`: This procedure adds a new person to the data structure, given their name, age, occupation, and hobbies.

The code then uses a `foreach` loop to iterate over the names of the people in the data structure and prints information about each person using the `printPersonInfo` procedure.

Finally, the code uses the `addPerson` procedure to add a new person to the data structure and then prints information about the new person using the `printPersonInfo` procedure.