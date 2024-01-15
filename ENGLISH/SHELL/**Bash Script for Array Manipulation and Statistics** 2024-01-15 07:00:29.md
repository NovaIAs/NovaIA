```shell
#!/bin/bash
# This script demonstrates the use of arrays, loops, and conditional statements in Bash.
# It generates random numbers, stores them in an array, and performs various operations on the array.

# Define an array to store the random numbers.
declare -a random_numbers

# Generate 10 random numbers between 1 and 100 and store them in the array.
for i in $(seq 1 10); do
    random_number=$((RANDOM % 100 + 1))
    random_numbers+=($random_number)
done

# Print the original array.
echo "Original array:"
echo "${random_numbers[@]}"

# Find the largest number in the array.
largest_number=${random_numbers[0]}
for number in "${random_numbers[@]}"; do
    if [[ $number -gt $largest_number ]]; then
        largest_number=$number
    fi
done

# Print the largest number.
echo "Largest number in the array: $largest_number"

# Find the smallest number in the array.
smallest_number=${random_numbers[0]}
for number in "${random_numbers[@]}"; do
    if [[ $number -lt $smallest_number ]]; then
        smallest_number=$number
    fi
done

# Print the smallest number.
echo "Smallest number in the array: $smallest_number"

# Calculate the sum of all the numbers in the array.
sum=0
for number in "${random_numbers[@]}"; do
    sum=$((sum + number))
done

# Print the sum of the numbers.
echo "Sum of all numbers in the array: $sum"

# Calculate the average of all the numbers in the array.
average=$((sum / ${#random_numbers[@]}))

# Print the average of the numbers.
echo "Average of all numbers in the array: $average"

# Sort the array in ascending order.
sorted_random_numbers=($(sort "${random_numbers[@]}"))

# Print the sorted array.
echo "Sorted array:"
echo "${sorted_random_numbers[@]}"

# Reverse the sorted array.
reversed_sorted_random_numbers=($(tac "${sorted_random_numbers[@]}"))

# Print the reversed sorted array.
echo "Reversed sorted array:"
echo "${reversed_sorted_random_numbers[@]}"
```

Explanation:

1. `declare -a random_numbers`: This line declares an array named `random_numbers`.

2. `for i in $(seq 1 10); do ... done`: This loop generates 10 random numbers between 1 and 100 using the `seq` command and stores them in the `random_numbers` array.

3. `echo "Original array:"` and `echo "${random_numbers[@]}"`: These lines print the original array.

4. `largest_number=${random_numbers[0]}`: This line initializes the `largest_number` variable with the first element of the `random_numbers` array.

5. `for number in "${random_numbers[@]}"; do ... done`: This loop iterates over each element of the `random_numbers` array.

6. `if [[ $number -gt $largest_number ]]; then ... fi`: Inside the loop, this conditional statement checks if the current number is greater than the current largest number. If it is, it updates the `largest_number` variable with the current number.

7. `echo "Largest number in the array: $largest_number"`: This line prints the largest number in the array.

8. Similar steps are taken to find the smallest number, calculate the sum, and calculate the average of the numbers in the array.

9. `sorted_random_numbers=($(sort "${random_numbers[@]}"))`: This line sorts the `random_numbers` array in ascending order using the `sort` command and stores the result in the `sorted_random_numbers` array.

10. `reversed_sorted_random_numbers=($(tac "${sorted_random_numbers[@]}"))`: This line reverses the `sorted_random_numbers` array using the `tac` command and stores the result in the `reversed_sorted_random_numbers` array.

11. `echo "Sorted array:"` and `echo "${sorted_random_numbers[@]}"`: These lines print the sorted array.

12. `echo "Reversed sorted array:"` and `echo "${reversed_sorted_random_numbers[@]}"`: These lines print the reversed sorted array.