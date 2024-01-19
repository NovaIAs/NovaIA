// This code is a solution to the problem of finding the longest common substring between two strings.

// The function takes two strings as input and returns the longest common substring.

func longestCommonSubstring(_ s1: String, _ s2: String) -> String {
    // Create a matrix to store the lengths of the longest common substrings of the prefixes of the two strings.

    var matrix = Array(repeating: Array(repeating: 0, count: s2.count), count: s1.count)

    // Initialize the first row and column of the matrix to 0.

    for i in 0..<s1.count {
        matrix[i][0] = 0
    }

    for j in 0..<s2.count {
        matrix[0][j] = 0
    }

    // Iterate over the strings and fill in the matrix.

    for i in 1..<s1.count {
        for j in 1..<s2.count {
            // If the characters at the current positions in the two strings are equal, then the length of the longest common substring is 1 plus the length of the longest common substring of the prefixes of the two strings up to the previous positions.

            if s1[i] == s2[j] {
                matrix[i][j] = matrix[i - 1][j - 1] + 1
            }

            // Otherwise, the length of the longest common substring is 0.

            else {
                matrix[i][j] = 0
            }
        }
    }

    // Find the maximum value in the matrix.

    var max = 0
    var maxI = 0
    var maxJ = 0

    for i in 0..<s1.count {
        for j in 0..<s2.count {
            if matrix[i][j] > max {
                max = matrix[i][j]
                maxI = i
                maxJ = j
            }
        }
    }

    // Construct the longest common substring.

    var substring = ""

    for i in maxI - max + 1...maxI {
        substring += String(s1[i])
    }

    return substring
}

// Example usage:

let s1 = "ABCDGHLQR"
let s2 = "AEDPHR"

let longestCommonSubstring = longestCommonSubstring(s1, s2)

print(longestCommonSubstring) // Output: "ADH"


// Explanation:

The code first creates a matrix to store the lengths of the longest common substrings of the prefixes of the two strings. The first row and column of the matrix are initialized to 0.

Then, the code iterates over the strings and fills in the matrix. If the characters at the current positions in the two strings are equal, then the length of the longest common substring is 1 plus the length of the longest common substring of the prefixes of the two strings up to the previous positions. Otherwise, the length of the longest common substring is 0.

The code then finds the maximum value in the matrix. This is the length of the longest common substring.

Finally, the code constructs the longest common substring by concatenating the characters from the two strings at the positions corresponding to the maximum value in the matrix.