```python

# This is a function that takes a list of numbers and returns the sum of all the numbers in the list.
def sum_of_numbers(numbers):
    total = 0
    for number in numbers:
        total += number
    return total


# This is a function that takes a string and returns the number of vowels in the string.
def count_vowels(string):
    vowels = ['a', 'e', 'i', 'o', 'u']
    count = 0
    for letter in string:
        if letter in vowels:
            count += 1
    return count


# This is a function that takes a list of words and returns the longest word in the list.
def longest_word(words):
    longest = ''
    for word in words:
        if len(word) > len(longest):
            longest = word
    return longest


# This is a function that takes a number and returns the factorial of that number.
def factorial(number):
    if number == 0:
        return 1
    else:
        return number * factorial(number - 1)


# This is a function that takes a string and returns a dictionary with the count of each character in the string.
def count_characters(string):
    characters = {}
    for character in string:
        if character in characters:
            characters[character] += 1
        else:
            characters[character] = 1
    return characters


# This is a function that takes a list of numbers and returns the average of all the numbers in the list.
def average_of_numbers(numbers):
    total = sum_of_numbers(numbers)
    count = len(numbers)
    average = total / count
    return average


# This is a function that takes a list of numbers and returns the median of all the numbers in the list.
def median_of_numbers(numbers):
    numbers.sort()
    count = len(numbers)
    if count % 2 == 0:
        median = (numbers[count // 2 - 1] + numbers[count // 2]) / 2
    else:
        median = numbers[count // 2]
    return median


# This is a function that takes a list of numbers and returns the mode of all the numbers in the list.
def mode_of_numbers(numbers):
    counts = {}
    for number in numbers:
        if number in counts:
            counts[number] += 1
        else:
            counts[number] = 1
    max_count = max(counts.values())
    modes = []
    for number, count in counts.items():
        if count == max_count:
            modes.append(number)
    return modes


# This is a function that takes a list of numbers and returns the range of all the numbers in the list.
def range_of_numbers(numbers):
    minimum = min(numbers)
    maximum = max(numbers)
    return maximum - minimum


# This is a function that takes a list of numbers and returns the variance of all the numbers in the list.
def variance_of_numbers(numbers):
    average = average_of_numbers(numbers)
    squared_differences = []
    for number in numbers:
        squared_differences.append((number - average) ** 2)
    variance = sum(squared_differences) / (len(numbers) - 1)
    return variance


# This is a function that takes a list of numbers and returns the standard deviation of all the numbers in the list.
def standard_deviation_of_numbers(numbers):
    variance = variance_of_numbers(numbers)
    standard_deviation = variance ** 0.5
    return standard_deviation


# This is a function that takes a list of numbers and returns the covariance of all the numbers in the list.
def covariance_of_numbers(numbers, numbers2):
    average1 = average_of_numbers(numbers)
    average2 = average_of_numbers(numbers2)
    squared_differences = []
    for i in range(len(numbers)):
        squared_differences.append((numbers[i] - average1) * (numbers2[i] - average2))
    covariance = sum(squared_differences) / (len(numbers) - 1)
    return covariance


# This is a function that takes a list of numbers and returns the correlation coefficient of all the numbers in the list.
def correlation_coefficient_of_numbers(numbers, numbers2):
    covariance = covariance_of_numbers(numbers, numbers2)
    standard_deviation1 = standard_deviation_of_numbers(numbers)
    standard_deviation2 = standard_deviation_of_numbers(numbers2)
    correlation_coefficient = covariance / (standard_deviation1 * standard_deviation2)
    return correlation_coefficient