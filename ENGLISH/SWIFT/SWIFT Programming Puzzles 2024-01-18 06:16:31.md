// Welcome to your SWIFT programming puzzle!

// Define a function that takes an array of numbers and returns the sum of all the numbers in the array.
func sumArray(numbers: [Int]) -> Int {
    var sum = 0
    for number in numbers {
        sum += number
    }
    return sum
}

// Define a function that takes a string and returns the number of vowels in the string.
func countVowels(string: String) -> Int {
    let vowels = "aeiouAEIOU"
    var count = 0
    for character in string {
        if vowels.contains(character) {
            count += 1
        }
    }
    return count
}

// Define a function that takes an array of strings and returns the longest string in the array.
func longestString(strings: [String]) -> String {
    var longest = ""
    for string in strings {
        if string.count > longest.count {
            longest = string
        }
    }
    return longest
}

// Define a function that takes a dictionary of strings and integers and returns the sum of all the integers in the dictionary.
func sumDictionary(dictionary: [String: Int]) -> Int {
    var sum = 0
    for (_, value) in dictionary {
        sum += value
    }
    return sum
}

// Define a function that takes a set of strings and returns a new set containing only the strings that start with the letter "A".
func filterStrings(strings: Set<String>) -> Set<String> {
    var filteredStrings: Set<String> = []
    for string in strings {
        if string.first == "A" {
            filteredStrings.insert(string)
        }
    }
    return filteredStrings
}

// Define a function that takes an array of integers and returns a new array containing only the prime numbers in the array.
func filterPrimeNumbers(numbers: [Int]) -> [Int] {
    var primeNumbers: [Int] = []
    for number in numbers {
        if isPrime(number) {
            primeNumbers.append(number)
        }
    }
    return primeNumbers
}

// Define a helper function to check if a number is prime.
func isPrime(_ number: Int) -> Bool {
    if number <= 1 {
        return false
    }
    for i in 2..<number {
        if number % i == 0 {
            return false
        }
    }
    return true
}

// Define a function that takes an array of tuples and returns a new array containing only the tuples that have a string as the first element.
func filterTuples(tuples: [(String, Int)]) -> [(String, Int)] {
    var filteredTuples: [(String, Int)] = []
    for tuple in tuples {
        if let string = tuple.0 as? String {
            filteredTuples.append((string, tuple.1))
        }
    }
    return filteredTuples
}

// Now, let's use these functions to solve some problems.

// Problem 1: Find the sum of all the numbers in the following array.
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let sum = sumArray(numbers: numbers)
print("Sum of all numbers in the array:", sum)

// Problem 2: Find the number of vowels in the following string.
let string = "Hello, world!"
let vowelCount = countVowels(string: string)
print("Number of vowels in the string:", vowelCount)

// Problem 3: Find the longest string in the following array.
let strings = ["apple", "banana", "cherry", "dog", "elephant", "fish"]
let longest = longestString(strings: strings)
print("Longest string in the array:", longest)

// Problem 4: Find the sum of all the integers in the following dictionary.
let dictionary = ["apple": 1, "banana": 2, "cherry": 3, "dog": 4, "elephant": 5, "fish": 6]
let sum = sumDictionary(dictionary: dictionary)
print("Sum of all integers in the dictionary:", sum)

// Problem 5: Find all the strings that start with the letter "A" in the following set.
let strings = Set(["apple", "banana", "cherry", "dog", "elephant", "fish", "aardvark", "ant", "antelope"])
let filteredStrings = filterStrings(strings: strings)
print("Strings that start with the letter \"A\":", filteredStrings)

// Problem 6: Find all the prime numbers in the following array.
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
let primeNumbers = filterPrimeNumbers(numbers: numbers)
print("Prime numbers in the array:", primeNumbers)

// Problem 7: Find all the tuples that have a string as the first element in the following array.
let tuples = [("apple", 1), ("banana", 2), ("cherry", 3), (4, "dog"), (5, "elephant"), (6, "fish")]
let filteredTuples = filterTuples(tuples: tuples)
print("Tuples that have a string as the first element:", filteredTuples)

// There you have it! This is just a small sample of what you can do with SWIFT.