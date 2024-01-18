**Procedure:** `is_anagram()`

**Purpose:** To determine if two strings are anagrams of each other.

**Input:** Two strings, `str1` and `str2`.

**Output:** A boolean value indicating whether `str1` and `str2` are anagrams of each other.

**Algorithm:**

1. Check if the length of `str1` and `str2` is the same. If not, they cannot be anagrams.
2. Create a dictionary to store the frequency of each character in `str1`.
3. Iterate over `str2` and subtract the count of each character from the dictionary created in step 2.
4. If any of the values in the dictionary become 0, remove the key from the dictionary.
5. If the dictionary is empty, `str1` and `str2` are anagrams. Otherwise, they are not.

**Example:**

```
is_anagram('hello', 'olleh') == True
is_anagram('hello', 'world') == False
```

**Explanation:**

The `is_anagram()` procedure takes two strings, `str1` and `str2`, as input and returns a boolean value indicating whether they are anagrams of each other.

The first step is to check if the length of `str1` and `str2` is the same. If not, they cannot be anagrams because they are not made up of the same number of characters.

If the length of `str1` and `str2` is the same, the next step is to create a dictionary to store the frequency of each character in `str1`. This is done by iterating over `str1` and adding each character to the dictionary, along with its frequency.

The next step is to iterate over `str2` and subtract the count of each character from the dictionary created in the previous step. This is done by getting the value for each character in `str2` from the dictionary and subtracting 1 from it. If the value becomes 0, the key is removed from the dictionary.

After iterating over `str2`, if the dictionary is empty, it means that all of the characters in `str1` were found in `str2` and that they occurred the same number of times. This means that `str1` and `str2` are anagrams of each other. Otherwise, they are not.