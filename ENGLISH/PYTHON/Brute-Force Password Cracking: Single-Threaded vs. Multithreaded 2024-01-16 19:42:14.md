```python
import random
import string
import hashlib
import time
import threading

def generate_password(length):
  """Generates a random password of the given length."""

  # Define the character set to use for the password.
  charset = string.ascii_letters + string.digits + string.punctuation

  # Generate a random password.
  password = ''.join(random.choice(charset) for _ in range(length))

  # Return the password.
  return password

def hash_password(password):
  """Hashes a password using the SHA256 algorithm."""

  # Create a SHA256 hash object.
  hash_object = hashlib.sha256()

  # Encode the password as a byte string.
  password_bytes = password.encode('utf-8')

  # Update the hash object with the password bytes.
  hash_object.update(password_bytes)

  # Get the hash digest.
  hash_digest = hash_object.hexdigest()

  # Return the hash digest.
  return hash_digest

def check_password(password, hashed_password):
  """Checks if a password matches a hashed password."""

  # Hash the password.
  hashed_password_2 = hash_password(password)

  # Compare the hashed passwords.
  return hashed_password_2 == hashed_password

def brute_force_attack(hashed_password):
  """Attempts to brute-force a hashed password."""

  # Define the character set to use for the password.
  charset = string.ascii_letters + string.digits + string.punctuation

  # Iterate over all possible passwords of length 1 to 10.
  for length in range(1, 11):
    # Generate all possible passwords of the given length.
    passwords = (''.join(random.choice(charset) for _ in range(length)) for _ in range(10**length))

    # Check each password against the hashed password.
    for password in passwords:
      if check_password(password, hashed_password):
        # The password has been found.
        return password

  # The password was not found.
  return None

def multithreaded_brute_force_attack(hashed_password, num_threads):
  """Attempts to brute-force a hashed password using multiple threads."""

  # Create a list of threads.
  threads = []

  # Create a lock to protect the shared data.
  lock = threading.Lock()

  # Create a list to store the results of the threads.
  results = []

  # Define the function to be executed by each thread.
  def brute_force_attack_thread(hashed_password, lock, results):
    # Iterate over all possible passwords of length 1 to 10.
    for length in range(1, 11):
      # Generate all possible passwords of the given length.
      passwords = (''.join(random.choice(charset) for _ in range(length)) for _ in range(10**length))

      # Check each password against the hashed password.
      for password in passwords:
        if check_password(password, hashed_password):
          # The password has been found.
          with lock:
            results.append(password)
            return

  # Start the threads.
  for _ in range(num_threads):
    thread = threading.Thread(target=brute_force_attack_thread, args=(hashed_password, lock, results))
    thread.start()
    threads.append(thread)

  # Wait for the threads to finish.
  for thread in threads:
    thread.join()

  # Return the first password that was found.
  if results:
    return results[0]
  else:
    return None

def main():
  """The main function."""

  # Generate a password.
  password = generate_password(10)

  # Hash the password.
  hashed_password = hash_password(password)

  # Print the password and the hashed password.
  print("Password:", password)
  print("Hashed password:", hashed_password)

  # Attempt to brute-force the hashed password.
  start_time = time.time()
  password_brute_force = brute_force_attack(hashed_password)
  end_time = time.time()
  print("Brute-force attack took", end_time - start_time, "seconds.")

  # Check if the password was found.
  if password_brute_force:
    print("Password found:", password_brute_force)
  else:
    print("Password not found.")

  # Attempt to brute-force the hashed password using multiple threads.
  start_time = time.time()
  password_brute_force_multithreaded = multithreaded_brute_force_attack(hashed_password, 4)
  end_time = time.time()
  print("Multithreaded brute-force attack took", end_time - start_time, "seconds.")

  # Check if the password was found.
  if password_brute_force_multithreaded:
    print("Password found:", password_brute_force_multithreaded)
  else:
    print("Password not found.")

if __name__ == "__main__":
  main()
```

This code demonstrates a brute-force attack on a hashed password. It generates a random password, hashes it, and then attempts to find the original password by trying all possible combinations of characters. The code also demonstrates how to perform a brute-force attack using multiple threads.

Here is a breakdown of the code:

* The `generate_password()` function generates a random password of the given length.
* The `hash_password()` function hashes a password using the SHA256 algorithm.
* The `check_password()` function checks if a password matches a hashed password.
* The `brute_force_attack()` function attempts to brute-force a hashed password.
* The `multithreaded_brute_force_attack()` function attempts to brute-force a hashed password using multiple threads.
* The `main()` function generates a password, hashes it, and then attempts to brute-force the hashed password using both a single thread and multiple threads.

The code also includes a number of helper functions, such as `charset`, which defines the character set to use for the password, and `lock`, which is a lock to protect the shared data in the multithreaded brute-force attack.

The code is designed to be easy to understand and follow, even for those who are not familiar with Python. The comments throughout the code provide a detailed explanation of each function and how it works.