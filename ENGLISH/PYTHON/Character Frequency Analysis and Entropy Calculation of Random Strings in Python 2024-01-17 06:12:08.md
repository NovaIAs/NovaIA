```python
import random
import string
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Generate a random string of 100 characters
def generate_random_string(length):
  """Generates a random string of the given length."""
  # The string is composed of lowercase and uppercase letters, digits, and punctuation marks
  characters = string.ascii_letters + string.digits + string.punctuation
  # The random module is used to generate a random sequence of characters from the string
  random_string = ''.join(random.choice(characters) for i in range(length))
  return random_string

# Generate a list of 100 random strings
random_strings = [generate_random_string(100) for i in range(100)]

# Calculate the frequency of each character in the list of strings
character_frequencies = {}
for random_string in random_strings:
  for character in random_string:
    if character not in character_frequencies:
      character_frequencies[character] = 0
    character_frequencies[character] += 1

# Sort the character frequencies in descending order
sorted_character_frequencies = sorted(character_frequencies.items(), key=lambda x: x[1], reverse=True)

# Print the top 10 most frequent characters
print("Top 10 most frequent characters:")
for character, frequency in sorted_character_frequencies[:10]:
  print(f"{character}: {frequency}")

# Generate a scatter plot of the character frequencies
plt.scatter([i for i in range(len(sorted_character_frequencies))], [frequency for character, frequency in sorted_character_frequencies])
plt.xlabel("Character")
plt.ylabel("Frequency")
plt.title("Frequency of Characters in Random Strings")
plt.show()

# Generate a bar plot of the character frequencies
sns.barplot(x=[character for character, frequency in sorted_character_frequencies], y=[frequency for character, frequency in sorted_character_frequencies])
plt.xlabel("Character")
plt.ylabel("Frequency")
plt.title("Frequency of Characters in Random Strings")
plt.show()

# Calculate the entropy of the list of strings
entropy = 0
for character, frequency in character_frequencies.items():
  probability = frequency / (len(random_strings) * 100)
  entropy += probability * math.log2(probability)

print(f"Entropy: {entropy}")
```

This code generates a list of 100 random strings, each of length 100. It then calculates the frequency of each character in the list of strings and sorts the characters in descending order based on their frequency. The code then prints the top 10 most frequent characters and generates a scatter plot and a bar plot of the character frequencies. Finally, the code calculates the entropy of the list of strings.

The code uses the `random` module to generate random strings, the `string` module to define the set of characters to use in the strings, the `math` module to calculate the entropy, the `numpy` module to perform numerical operations, the `pandas` module to create a DataFrame of the character frequencies, and the `matplotlib.pyplot` and `seaborn` modules to generate the plots.