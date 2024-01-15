```python
import random
import string
import hashlib
import time
import os

# Define a function to generate a random string of a given length
def generate_random_string(length):
  """Generates a random string of a given length."""

  # Create a list of all possible characters
  characters = string.ascii_letters + string.digits + string.punctuation

  # Generate a random string of the desired length
  random_string = ''.join(random.choice(characters) for i in range(length))

  # Return the random string
  return random_string

# Define a function to hash a string using the SHA256 algorithm
def hash_string(string):
  """Hashes a string using the SHA256 algorithm."""

  # Create a SHA256 hash object
  sha256 = hashlib.sha256()

  # Encode the string as bytes
  string_bytes = string.encode('utf-8')

  # Update the hash object with the string bytes
  sha256.update(string_bytes)

  # Get the hash digest
  hash_digest = sha256.hexdigest()

  # Return the hash digest
  return hash_digest

# Define a function to measure the execution time of a function
def measure_execution_time(func):
  """Measures the execution time of a function."""

  # Get the start time
  start_time = time.time()

  # Call the function
  func()

  # Get the end time
  end_time = time.time()

  # Calculate the execution time
  execution_time = end_time - start_time

  # Return the execution time
  return execution_time

# Define a function to create a directory if it doesn't exist
def create_directory(directory):
  """Creates a directory if it doesn't exist."""

  # Check if the directory exists
  if not os.path.exists(directory):

    # Create the directory
    os.makedirs(directory)

# Generate a random string of length 100
random_string = generate_random_string(100)

# Hash the random string
hashed_string = hash_string(random_string)

# Measure the execution time of the hashing function
execution_time = measure_execution_time(lambda: hash_string(random_string))

# Create a directory called 'data'
create_directory('data')

# Save the random string and hashed string to a file
with open('data/random_string.txt', 'w') as f:
  f.write(random_string)

with open('data/hashed_string.txt', 'w') as f:
  f.write(hashed_string)

# Save the execution time to a file
with open('data/execution_time.txt', 'w') as f:
  f.write(str(execution_time))

# Print the random string, hashed string, and execution time
print('Random String:', random_string)
print('Hashed String:', hashed_string)
print('Execution Time:', execution_time)
```

This code performs the following tasks:

1. Generates a random string of length 100.
2. Hashes the random string using the SHA256 algorithm.
3. Measures the execution time of the hashing function.
4. Creates a directory called 'data' if it doesn't exist.
5. Saves the random string and hashed string to a file in the 'data' directory.
6. Saves the execution time to a file in the 'data' directory.
7. Prints the random string, hashed string, and execution time to the console.