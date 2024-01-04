```groovy
class ComplexGroovyCode {

    // Method to calculate the factorial of a number
    def factorial(int n) {
        if (n == 0) {
            return 1
        } else {
            return n * factorial(n - 1)
        }
    }

    // Method to check if a number is prime
    def isPrime(int n) {
        if (n <= 1) {
            return false
        } else if (n <= 3) {
            return true
        } else if (n % 2 == 0 || n % 3 == 0) {
            return false
        } else {
            def i = 5
            def w = 2
            while (i * i <= n) {
                if (n % i == 0 || n % (i + 2) == 0) {
                    return false
                }
                i += w
                w = 6 - w
            }
            return true
        }
    }

    // Method to find the greatest common divisor of two numbers
    def gcd(int a, int b) {
        if (b == 0) {
            return a
        } else {
            return gcd(b, a % b)
        }
    }

    // Method to find the least common multiple of two numbers
    def lcm(int a, int b) {
        return (a * b) / gcd(a, b)
    }

    // Method to generate a random number between two numbers
    def random(int min, int max) {
        return (int) (Math.random() * (max - min + 1)) + min
    }

    // Method to shuffle an array of elements
    def shuffle(def array) {
        for (int i = 0; i < array.size(); i++) {
            def j = random(i, array.size() - 1)
            def temp = array[i]
            array[i] = array[j]
            array[j] = temp
        }
        return array
    }

    // Method to sort an array of elements in ascending order
    def sortAscending(def array) {
        array.sort()
        return array
    }

    // Method to sort an array of elements in descending order
    def sortDescending(def array) {
        array.sort { a, b -> b <=> a }
        return array
    }

    // Method to find the sum of an array of numbers
    def sum(def array) {
        return array.sum()
    }

    // Method to find the average of an array of numbers
    def average(def array) {
        return sum(array) / array.size()
    }

    // Method to find the maximum value in an array of numbers
    def max(def array) {
        return array.max()
    }

    // Method to find the minimum value in an array of numbers
    def min(def array) {
        return array.min()
    }

    // Method to join an array of elements into a string
    def join(def array, String separator) {
        return array.join(separator)
    }

    // Method to split a string into an array of substrings
    def split(String string, String separator) {
        return string.split(separator)
    }

    // Method to reverse a string
    def reverse(String string) {
        return string.reverse()
    }

    // Method to check if a string contains a substring
    def contains(String string, String substring) {
        return string.contains(substring)
    }

    // Method to replace all occurrences of a substring in a string with another substring
    def replaceAll(String string, String oldSubstring, String newSubstring) {
        return string.replaceAll(oldSubstring, newSubstring)
    }

    // Method to convert a string to uppercase
    def toUpperCase(String string) {
        return string.toUpperCase()
    }

    // Method to convert a string to lowercase
    def toLowerCase(String string) {
        return string.toLowerCase()
    }

    // Method to trim whitespace from a string
    def trim(String string) {
        return string.trim()
    }

    // Method to pad a string with a specified character
    def padLeft(String string, int length, String padChar) {
        return string.padLeft(length, padChar)
    }

    // Method to pad a string with a specified character
    def padRight(String string, int length, String padChar) {
        return string.padRight(length, padChar)
    }

    // Method to format a number as a string with a specified number of decimal places
    def formatNumber(double number, int decimalPlaces) {
        return String.format("%." + decimalPlaces + "f", number)
    }

    // Method to format a date as a string with a specified format
    def formatDate(Date date, String format) {
        return date.format(format)
    }

    // Method to parse a string into a date with a specified format
    def parseDate(String string, String format) {
        return Date.parse(format, string)
    }
}
```

This code contains a variety of complex and differentiated methods that perform various operations on numbers, strings, and arrays. It includes methods for calculating the factorial of a number, checking if a number is prime, finding the greatest common divisor and least common multiple of two numbers, generating a random number, shuffling an array, sorting an array in ascending or descending order, finding the sum, average, maximum, and minimum values in an array, joining an array of elements into a string, splitting a string into an array of substrings, reversing a string, checking if a string contains a substring, replacing all occurrences of a substring in a string with another substring, converting a string to uppercase or lowercase, trimming whitespace from a string, padding a string with a specified character, formatting a number as a string with a specified number of decimal places, formatting a date as a string with a specified format, and parsing a string into a date with a specified format.