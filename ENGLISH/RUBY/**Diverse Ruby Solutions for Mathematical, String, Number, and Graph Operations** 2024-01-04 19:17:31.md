```ruby
# This Ruby code demonstrates a complex and differentiated solution for various use cases.

# Define a reusable module for mathematical operations.
module MathOperations
  # Define a method to calculate the factorial of a given number.
  def factorial(number)
    if number == 0
      1
    else
      number * factorial(number - 1)
    end
  end

  # Define a method to calculate the greatest common divisor of two numbers.
  def gcd(a, b)
    if b == 0
      a
    else
      gcd(b, a % b)
    end
  end

  # Define a method to check if a given number is prime.
  def prime?(number)
    return false if number < 2

    (2...number).each do |i|
      return false if number % i == 0
    end

    true
  end
end

# Include the MathOperations module to use its methods.
include MathOperations

# Calculate the factorial of a given number.
puts "Factorial of 5: #{factorial(5)}"

# Calculate the greatest common divisor of two numbers.
puts "Greatest Common Divisor of 12 and 18: #{gcd(12, 18)}"

# Check if a given number is prime.
puts "Is 13 prime? #{prime?(13)}"
puts "Is 15 prime? #{prime?(15)}"

# Define a method to generate a random string of a given length.
def generate_random_string(length)
  charset = ('a'..'z').to_a + ('A'..'Z').to_a + ('0'..'9').to_a
  string = ''
  length.times { string << charset[rand(charset.length)] }
  string
end

# Generate a random string of length 10 and display it.
puts "Random String of Length 10: #{generate_random_string(10)}"

# Define a method to convert a decimal number to its binary representation.
def decimal_to_binary(number)
  binary = ''
  while number > 0
    binary = (number % 2).to_s + binary
    number /= 2
  end
  binary
end

# Convert a given decimal number to binary and display it.
puts "Binary Representation of 13: #{decimal_to_binary(13)}"

# Define a method to reverse a given string.
def reverse_string(string)
  reversed = ''
  string.each_char { |char| reversed = char + reversed }
  reversed
end

# Reverse a given string and display it.
puts "Reversed String of 'Hello': #{reverse_string('Hello')}"

# Define a method to count the occurrences of a given character in a string.
def count_character(string, char)
  count = 0
  string.each_char { |c| count += 1 if c == char }
  count
end

# Count the occurrences of a given character in a string and display the result.
puts "Occurrences of 'e' in 'Hello': #{count_character('Hello', 'e')}"

# Define a method to find the longest common substring of two strings.
def longest_common_substring(string1, string2)
  substring = ''
  0.upto(string1.length - 1) do |i|
    1.upto(string1.length - i) do |j|
      substring = string1[i, j] if substring.length < j && string2.include?(substring)
    end
  end
  substring
end

# Find the longest common substring of two strings and display it.
puts "Longest Common Substring of 'ABCD' and 'ABEF': #{longest_common_substring('ABCD', 'ABEF')}"

# Define a method to find the shortest path in a graph.
def shortest_path(graph, start, finish)
  queue = [[start, 0]]
  visited = Set.new
  until queue.empty?
    current, distance = queue.shift
    return distance if current == finish

    visited.add(current)
    graph[current].each do |neighbor|
      unless visited.include?(neighbor)
        queue << [neighbor, distance + 1]
      end
    end
  end

  -1 # No path found
end

# Define a graph and find the shortest path between two nodes.
graph = {
  'A' => ['B', 'C'],
  'B' => ['D', 'E'],
  'C' => ['F'],
  'D' => ['G'],
  'E' => ['H'],
  'F' => ['I'],
  'G' => ['J'],
  'H' => ['K'],
  'I' => ['L'],
  'J' => ['M'],
  'K' => ['N'],
  'L' => ['O'],
  'M' => ['P'],
  'N' => ['Q'],
  'O' => ['R'],
  'P' => ['S'],
  'Q' => ['T'],
  'R' => ['U'],
  'S' => ['V'],
  'T' => ['W'],
  'U' => ['X'],
  'V' => ['Y'],
  'W' => ['Z'],
  'X' => [],
  'Y' => [],
  'Z' => []
}

puts "Shortest Path from 'A' to 'Z': #{shortest_path(graph, 'A', 'Z')}"
```

This Ruby code showcases a wide range of advanced and practical functionalities, including:

- Mathematical operations: calculating factorial, greatest common divisor, and checking for prime numbers.
- String manipulation: generating random strings, reversing strings, and counting character occurrences.
- Number conversion: converting decimal numbers to binary representation.
- String comparison: finding the longest common substring of two strings.
- Graph traversal: finding the shortest path in a graph using breadth-first search.

The code includes detailed comments to explain the purpose and functionality of each section, making it easy to understand and adapt for various applications.