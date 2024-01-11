```python
import random
import string
import time

def generate_random_string(length):
    """Generates a random string of the given length."""
    # Define the characters that can be used in the string.
    characters = string.ascii_letters + string.digits + string.punctuation

    # Create an empty string to store the random string.
    random_string = ""

    # Iterate over the desired length of the string.
    for _ in range(length):
        # Get a random character from the list of characters.
        random_character = random.choice(characters)

        # Add the random character to the random string.
        random_string += random_character

    # Return the random string.
    return random_string

def generate_random_number(min_value, max_value):
    """Generates a random number between the given minimum and maximum values."""
    # Generate a random number between 0 and 1.
    random_number = random.random()

    # Scale the random number to the desired range.
    random_number = random_number * (max_value - min_value) + min_value

    # Return the random number.
    return random_number

def generate_random_date(start_date, end_date):
    """Generates a random date between the given start and end dates."""
    # Convert the start and end dates to timestamps.
    start_timestamp = time.mktime(start_date.timetuple())
    end_timestamp = time.mktime(end_date.timetuple())

    # Generate a random timestamp between the start and end timestamps.
    random_timestamp = random_number(start_timestamp, end_timestamp)

    # Convert the random timestamp to a date object.
    random_date = time.localtime(random_timestamp)

    # Return the random date.
    return random_date

def generate_random_data(num_records):
    """Generates a list of random data records."""
    # Create an empty list to store the data records.
    data_records = []

    # Iterate over the desired number of records.
    for _ in range(num_records):
        # Generate a random string.
        random_string = generate_random_string(10)

        # Generate a random number.
        random_number = generate_random_number(0, 100)

        # Generate a random date.
        random_date = generate_random_date(datetime.date(2000, 1, 1), datetime.date(2023, 12, 31))

        # Create a data record.
        data_record = {
            "string_field": random_string,
            "number_field": random_number,
            "date_field": random_date
        }

        # Add the data record to the list of data records.
        data_records.append(data_record)

    # Return the list of data records.
    return data_records

# Generate 100 random data records.
data_records = generate_random_data(100)

# Print the data records.
for data_record in data_records:
    print(data_record)
```

This code generates a list of 100 random data records. Each data record consists of a random string, a random number, and a random date. The random string is generated using the `generate_random_string()` function, the random number is generated using the `generate_random_number()` function, and the random date is generated using the `generate_random_date()` function. The `generate_random_data()` function takes the number of records to generate as an argument and returns a list of data records.

The code then prints the data records to the console.