```
#!/bin/bash

# This script is a complex example of shell scripting in English. It demonstrates the use of various shell features, including variables, arrays, loops, conditionals, and functions.

# Define some variables.
name="John Doe"
age=30
city="New York City"

# Define an array.
fruits=("apple" "banana" "orange" "grape" "strawberry")

# Print the values of the variables and array to the console.
echo "Name: $name"
echo "Age: $age"
echo "City: $city"
echo "Fruits: ${fruits[@]}"

# Use a loop to iterate over the array and print each element to the console.
for fruit in "${fruits[@]}"; do
  echo "Fruit: $fruit"
done

# Use a conditional statement to check if the user is over the age of 21.
if [ $age -gt 21 ]; then
  echo "You are old enough to drink alcohol."
else
  echo "You are not old enough to drink alcohol."
fi

# Define a function to calculate the area of a rectangle.
function calculate_area() {
  length=$1
  width=$2
  area=$((length * width))
  echo "The area of the rectangle is $area square units."
}

# Call the function to calculate the area of a rectangle with a length of 10 and a width of 5.
calculate_area 10 5

# Exit the script with a status code of 0 to indicate success.
exit 0
```

Explanation:

1. **Variables:** Variables are used to store data in shell scripts. They are declared using the `=` operator. In this script, we define several variables, including `name`, `age`, `city`, and `fruits`.

2. **Arrays:** Arrays are used to store a collection of data items. They are declared using the `()` operator. In this script, we define an array called `fruits` that contains a list of fruits.

3. **Loops:** Loops are used to repeat a block of code a specified number of times. In this script, we use a `for` loop to iterate over the `fruits` array and print each element to the console.

4. **Conditionals:** Conditionals are used to check if a condition is true or false. In this script, we use an `if` statement to check if the user is over the age of 21.

5. **Functions:** Functions are used to group related code together and can be called from other parts of the script. In this script, we define a function called `calculate_area()` that calculates the area of a rectangle.

6. **Exit Code:** The `exit` command is used to exit the script with a specified status code. In this script, we use the `exit 0` command to exit the script with a status code of 0, which indicates success.