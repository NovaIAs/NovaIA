```ruby
# This Ruby code demonstrates a complex and differentiated program that involves various programming concepts.

# Defining a method that takes an array of numbers and returns the sum of its elements
def sum_array(numbers)
  total = 0
  numbers.each do |number|
    total += number
  end
  total
end

# Defining a method that takes a string and returns a reversed version of it
def reverse_string(string)
  string.reverse
end

# Defining a method that takes a hash and returns a new hash with the keys and values swapped
def swap_keys_and_values(hash)
  swapped_hash = {}
  hash.each do |key, value|
    swapped_hash[value] = key
  end
  swapped_hash
end

# Defining a method that takes an array of strings and returns a new array with the strings sorted in descending order
def sort_strings_descending(strings)
  strings.sort { |a, b| b <=> a }
end

# Defining a method that takes a number and returns its factorial
def factorial(number)
  if number == 0
    1
  else
    number * factorial(number - 1)
  end
end

# Defining a method that takes an array of arrays and returns a new array with the elements of the sub-arrays flattened into a single array
def flatten_array(array)
  array.flatten
end

# Defining a method that takes two strings and returns the longest common subsequence between them
def longest_common_subsequence(string1, string2)
  matrix = Array.new(string1.length + 1) { Array.new(string2.length + 1, 0) }

  (1..string1.length).each do |i|
    (1..string2.length).each do |j|
      if string1[i - 1] == string2[j - 1]
        matrix[i][j] = matrix[i - 1][j - 1] + 1
      else
        matrix[i][j] = [matrix[i - 1][j], matrix[i][j - 1]].max
      end
    end
  end

  subsequence = ""
  i = string1.length
  j = string2.length
  while i > 0 && j > 0
    if string1[i - 1] == string2[j - 1]
      subsequence = string1[i - 1] + subsequence
      i -= 1
      j -= 1
    elsif matrix[i - 1][j] > matrix[i][j - 1]
      i -= 1
    else
      j -= 1
    end
  end

  subsequence
end

# Defining a method that takes a string and returns true if it is a palindrome, false otherwise
def is_palindrome(string)
  string == string.reverse
end

# Defining a method that takes a number and returns true if it is prime, false otherwise
def is_prime(number)
  return false if number <= 1

  (2...number).none? { |divisor| number % divisor == 0 }
end

# Defining a method that takes an array of numbers and returns the mode (the most frequently occurring value) of the array
def mode(array)
  frequency = Hash.new(0)
  array.each do |number|
    frequency[number] += 1
  end

  max_frequency = frequency.values.max
  mode_values = frequency.select { |number, count| count == max_frequency }.keys
  mode_values
end

# Defining a method that takes a string and returns a hash with the frequency of each character in the string
def character_frequency(string)
  frequency = Hash.new(0)
  string.each_char do |char|
    frequency[char] += 1
  end
  frequency
end

# Defining a method that takes a string and returns a hash with the number of occurrences of each word in the string
def word_frequency(string)
  words = string.split
  frequency = Hash.new(0)
  words.each do |word|
    frequency[word] += 1
  end
  frequency
end

# Defining a method that takes an array of hashes and returns a new array with the hashes merged based on a common key
def merge_hashes_by_key(hashes, key)
  merged_hash = {}
  hashes.each do |hash|
    if merged_hash.has_key?(hash[key])
      merged_hash[hash[key]].merge!(hash)
    else
      merged_hash[hash[key]] = hash
    end
  end
  merged_hash
end

# Defining a method that takes an array of objects and returns a new array with the objects sorted by a specified attribute
def sort_objects_by_attribute(objects, attribute)
  objects.sort_by { |object| object.send(attribute) }
end

# Defining a method that takes a string and returns a hash with the frequency of each n-gram (sequence of n consecutive characters) in the string
def ngram_frequency(string, n)
  ngrams = []
  (0...string.length - n + 1).each do |i|
    ngrams << string[i, n]
  end

  frequency = Hash.new(0)
  ngrams.each do |ngram|
    frequency[ngram] += 1
  end
  frequency
end

# Defining a method that takes a string and returns a new string with the first and last characters swapped
def swap_first_and_last_characters(string)
  first_char = string[0]
  last_char = string[-1]
  string[0] = last_char
  string[-1] = first_char
  string
end

# Defining a method that takes an array of strings and returns a new array with the strings converted to uppercase
def convert_to_uppercase(strings)
  strings.map(&:upcase)
end

# Defining a method that takes an array of integers and returns a new array with the integers sorted in ascending order
def sort_integers_ascending(integers)
  integers.sort
end

# Defining a method that takes an array of hashes and returns a new array with the hashes grouped by a common key
def group_hashes_by_key(hashes, key)
  grouped_hashes = Hash.new { |hash, key| hash[key] = [] }
  hashes.each do |hash|
    grouped_hashes[hash[key]] << hash
  end
  grouped_hashes
end

# Defining a method that takes a string and returns a new string with the vowels removed
def remove_vowels(string)
  string.delete('aeiouAEIOU')
end

# Defining a method that takes an array of numbers and returns a new array with the numbers rounded to the nearest integer
def round_numbers(numbers)
  numbers.map { |number| number.round }
end

# Defining a method that takes a string and returns a new string with the words reversed
def reverse_words(string)
  words = string.split
  words.reverse.join(' ')
end

# Defining a method that takes an array of strings and returns a new array with the strings joined together into a single string
def join_strings(strings)
  strings.join
end

# Defining a method that takes a string and returns a new string with the duplicate characters removed
def remove_duplicates(string)
  string.chars.uniq.join
end

# Defining a method that takes an array of numbers and returns a new array with the numbers filtered based on a condition
def filter_numbers(numbers, condition)
  numbers.select { |number| condition.call(number) }
end

# Defining a method that takes a string and returns a new string with the characters in alphabetical order
def sort_characters(string)
  string.chars.sort.join
end

# Defining a method that takes an array of strings and returns a new array with the strings sorted by length
def sort_strings_by_length(strings)
  strings.sort_by { |string| string.length }
end

# Defining a method that takes a string and returns a new string with the spaces removed
def remove_spaces(string)
  string.delete(' ')
end

# Defining a method that takes an array of numbers and returns a new array with the numbers in reverse order
def reverse_numbers(numbers)
  numbers.reverse
end

# Defining a method that takes a string and returns a new string with the case of the characters swapped
def swap_case(string)
  string.swapcase
end

# Defining a method that takes an array of strings and returns a new array with the strings capitalized
def capitalize_strings(strings)
  strings.map(&:capitalize)
end

# Defining a method that takes a string and returns a new string with the punctuation removed
def remove_punctuation(string)
  string.gsub(/[[:punct:]]