```shell
#!/bin/sh

# Welcome message
echo "Welcome to the Shell Scripting Masterclass!"
echo

# Define some variables
name="John Doe"
age=30
city="New York City"

# Print the values of the variables
echo "Your name is $name."
echo "Your age is $age."
echo "You live in $city."
echo

# Ask the user for their name
echo "What is your name?"
read name

# Ask the user for their age
echo "How old are you?"
read age

# Ask the user for their city
echo "Where do you live?"
read city

# Print the values of the variables
echo "Your name is $name."
echo "Your age is $age."
echo "You live in $city."
echo

# Check if the user is over 18 years old
if [ $age -gt 18 ]; then
  echo "You are old enough to drink alcohol."
else
  echo "You are not old enough to drink alcohol."
fi

# Check if the user lives in New York City
if [ $city = "New York City" ]; then
  echo "You live in the Big Apple!"
else
  echo "You do not live in New York City."
fi

# Loop through a list of numbers
for i in 1 2 3 4 5; do
  echo "The number is $i."
done

# Loop through a list of files in the current directory
for file in *; do
  echo "The file is $file."
done

# Create a function to calculate the factorial of a number
factorial() {
  if [ $1 -eq 0 ]; then
    return 1
  else
    return `expr $1 \* $(factorial $(($1 - 1)))`
  fi
}

# Call the factorial function
result=$(factorial 5)
echo "The factorial of 5 is $result."

# Exit the script
exit 0
```

**Explanation:**

1. The script starts with a welcome message.

2. Then, it defines some variables: `name`, `age`, and `city`.

3. It prints the values of the variables.

4. Then, it asks the user for their name, age, and city.

5. It prints the values of the variables again.

6. Then, it checks if the user is over 18 years old. If they are, it prints a message saying that they are old enough to drink alcohol. Otherwise, it prints a message saying that they are not old enough to drink alcohol.

7. Then, it checks if the user lives in New York City. If they do, it prints a message saying that they live in the Big Apple. Otherwise, it prints a message saying that they do not live in New York City.

8. Then, it loops through a list of numbers from 1 to 5 and prints each number.

9. Then, it loops through a list of files in the current directory and prints each file.

10. Then, it defines a function called `factorial()` that calculates the factorial of a number.

11. Then, it calls the `factorial()` function with the number 5 and prints the result.

12. Finally, the script exits.