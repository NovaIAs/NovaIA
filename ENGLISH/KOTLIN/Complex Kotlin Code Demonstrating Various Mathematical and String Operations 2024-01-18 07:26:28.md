```kotlin
// This is a complex Kotlin code that performs various operations and demonstrates different features of the language.

// Function to calculate the factorial of a number using recursion.
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Function to check if a number is prime.
fun isPrime(n: Int): Boolean {
    if (n <= 1) {
        return false
    }
    for (i in 2 until n) {
        if (n % i == 0) {
            return false
        }
    }
    return true
}

// Function to find the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.
fun gcd(a: Int, b: Int): Int {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

// Function to find the least common multiple (LCM) of two numbers.
fun lcm(a: Int, b: Int): Int {
    return (a * b) / gcd(a, b)
}

// Function to generate a random number between two given numbers.
fun generateRandomNumber(min: Int, max: Int): Int {
    return (min + Random().nextInt(max - min + 1))
}

// Function to calculate the area of a triangle using Heron's formula.
fun triangleArea(a: Double, b: Double, c: Double): Double {
    val s = (a + b + c) / 2
    return Math.sqrt(s * (s - a) * (s - b) * (s - c))
}

// Function to calculate the volume of a sphere using the formula 4/3 * π * r^3.
fun sphereVolume(radius: Double): Double {
    return (4.0 / 3.0) * Math.PI * Math.pow(radius, 3.0)
}

// Function to convert a temperature from Fahrenheit to Celsius.
fun fahrenheitToCelsius(fahrenheit: Double): Double {
    return (fahrenheit - 32) * 5 / 9
}

// Function to convert a temperature from Celsius to Fahrenheit.
fun celsiusToFahrenheit(celsius: Double): Double {
    return (celsius * 9 / 5) + 32
}

// Function to check if a string is a palindrome (reads the same forwards and backwards).
fun isPalindrome(str: String): Boolean {
    return str.equals(str.reversed(), ignoreCase = true)
}

// Function to reverse a string.
fun reverseString(str: String): String {
    return str.reversed()
}

// Function to remove duplicate characters from a string.
fun removeDuplicates(str: String): String {
    return str.toCharArray().distinct().joinToString("")
}

// Function to count the number of occurrences of a character in a string.
fun countCharacterOccurrences(str: String, char: Char): Int {
    return str.count { it == char }
}

// Function to find the longest common subsequence (LCS) of two strings using dynamic programming.
fun longestCommonSubsequence(str1: String, str2: String): String {
    val lcs = Array(str1.length + 1) { IntArray(str2.length + 1) }

    for (i in 0 until str1.length) {
        for (j in 0 until str2.length) {
            if (str1[i] == str2[j]) {
                lcs[i + 1][j + 1] = lcs[i][j] + 1
            } else {
                lcs[i + 1][j + 1] = Math.max(lcs[i + 1][j], lcs[i][j + 1])
            }
        }
    }

    var lcsStr = ""
    var i = str1.length
    var j = str2.length

    while (i > 0 && j > 0) {
        if (str1[i - 1] == str2[j - 1]) {
            lcsStr = str1[i - 1] + lcsStr
            i--
            j--
        } else {
            if (lcs[i - 1][j] > lcs[i][j - 1]) {
                i--
            } else {
                j--
            }
        }
    }

    return lcsStr
}

// Function to find the shortest common supersequence (SCS) of two strings using dynamic programming.
fun shortestCommonSupersequence(str1: String, str2: String): String {
    val scs = Array(str1.length + 1) { IntArray(str2.length + 1) }

    for (i in 0 until str1.length) {
        scs[i + 1][0] = i + 1
    }
    for (j in 0 until str2.length) {
        scs[0][j + 1] = j + 1
    }

    for (i in 1 until str1.length + 1) {
        for (j in 1 until str2.length + 1) {
            if (str1[i - 1] == str2[j - 1]) {
                scs[i][j] = scs[i - 1][j - 1] + 1
            } else {
                scs[i][j] = Math.min(scs[i - 1][j], scs[i][j - 1]) + 1
            }
        }
    }

    var scsStr = ""
    var i = str1.length
    var j = str2.length

    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && str1[i - 1] == str2[j - 1]) {
            scsStr = str1[i - 1] + scsStr
            i--
            j--
        } else if (j == 0 || (i > 0 && scs[i - 1][j] < scs[i][j])) {
            scsStr = str1[i - 1] + scsStr
            i--
        } else {
            scsStr = str2[j - 1] + scsStr
            j--
        }
    }

    return scsStr
}

// Function to find all permutations of a string using backtracking.
fun permutations(str: String): List<String> {
    val result = mutableListOf<String>()
    val visited = BooleanArray(str.length) { false }

    fun permute(suffix: String) {
        if (suffix.isEmpty()) {
            result.add(str)
            return
        }

        for (i in 0 until str.length) {
            if (visited[i]) {
                continue
            }

            visited[i] = true
            permute(str.substring(0, i) + suffix + str.substring(i + 1))
            visited[i] = false
        }
    }

    permute("")
    return result
}

// Function to find all combinations of a set of elements.
fun combinations(elements: List<Int>, r: Int): List<List<Int>> {
    val result = mutableListOf<List<Int>>()

    fun combine(index: Int, combination: MutableList<Int>) {
        if (combination.size == r) {
            result.