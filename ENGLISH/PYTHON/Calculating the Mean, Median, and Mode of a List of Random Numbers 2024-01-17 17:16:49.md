```python
import random

# Define a function to generate a random number between two values.
def generate_random_number(min_value, max_value):
    """Generates a random number between two values.

    Args:
    min_value: The minimum value of the random number.
    max_value: The maximum value of the random number.

    Returns:
    A random number between min_value and max_value.
    """

    return random.uniform(min_value, max_value)


# Define a function to generate a list of random numbers.
def generate_random_list(length, min_value, max_value):
    """Generates a list of random numbers.

    Args:
    length: The length of the list.
    min_value: The minimum value of the random numbers.
    max_value: The maximum value of the random numbers.

    Returns:
    A list of random numbers.
    """

    random_list = []
    for _ in range(length):
        random_list.append(generate_random_number(min_value, max_value))
    return random_list


# Define a function to calculate the mean of a list of numbers.
def calculate_mean(numbers):
    """Calculates the mean of a list of numbers.

    Args:
    numbers: The list of numbers.

    Returns:
    The mean of the list of numbers.
    """

    return sum(numbers) / len(numbers)


# Define a function to calculate the median of a list of numbers.
def calculate_median(numbers):
    """Calculates the median of a list of numbers.

    Args:
    numbers: The list of numbers.

    Returns:
    The median of the list of numbers.
    """

    sorted_numbers = sorted(numbers)
    length = len(sorted_numbers)
    if length % 2 == 0:
        return (sorted_numbers[length // 2 - 1] + sorted_numbers[length // 2]) / 2
    else:
        return sorted_numbers[length // 2]


# Define a function to calculate the mode of a list of numbers.
def calculate_mode(numbers):
    """Calculates the mode of a list of numbers.

    Args:
    numbers: The list of numbers.

    Returns:
    The mode of the list of numbers.
    """

    counts = {}
    for number in numbers:
        if number not in counts:
            counts[number] = 0
        counts[number] += 1
    max_count = max(counts.values())
    modes = [number for number, count in counts.items() if count == max_count]
    return modes


# Generate a list of 100 random numbers between 0 and 100.
random_list = generate_random_list(100, 0, 100)

# Calculate the mean, median, and mode of the list of random numbers.
mean = calculate_mean(random_list)
median = calculate_median(random_list)
mode = calculate_mode(random_list)

# Print the results.
print("The mean of the list is:", mean)
print("The median of the list is:", median)
print("The mode of the list is:", mode)
```

This code generates a list of 100 random numbers between 0 and 100, and then calculates the mean, median, and mode of the list. The mean is the average of the numbers in the list, the median is the middle value of the list when sorted, and the mode is the value that occurs most frequently in the list. The code uses a number of helper functions to generate the random numbers and calculate the mean, median, and mode. The results are then printed to the console.