```julia
# Dynamic Programming in Julia: Longest Common Subsequence (LCS)

# Define the function to calculate the length of the LCS
function lcs(str1, str2)
    # Initialize a matrix to store the LCS lengths
    dp_table = zeros(Int64, length(str1) + 1, length(str2) + 1)

    # Iterate through the characters of the first string
    for i in 1:length(str1)
        # Iterate through the characters of the second string
        for j in 1:length(str2)
            # If the current characters are equal, the LCS length is one greater
            # than the LCS length of the previous characters
            if str1[i] == str2[j]
                dp_table[i, j] = dp_table[i - 1, j - 1] + 1
            # Otherwise, the LCS length is the maximum of the LCS lengths of the
            # previous characters
            else
                dp_table[i, j] = max(dp_table[i - 1, j], dp_table[i, j - 1])
            end
        end
    end

    # Return the LCS length
    return dp_table[end, end]
end

# Define the function to reconstruct the LCS
function reconstruct_lcs(str1, str2, dp_table)
    # Initialize an empty string to store the LCS
    lcs = ""

    # Start from the bottom-right corner of the DP table
    i = length(str1)
    j = length(str2)

    # While we are not at the top-left corner of the DP table
    while i > 0 && j > 0
        # If the current characters are equal, add them to the LCS and move
        # diagonally up in the DP table
        if str1[i] == str2[j]
            lcs = str1[i] * lcs
            i -= 1
            j -= 1
        # Otherwise, move up or left in the DP table, depending on which
        # has a greater LCS length
        else
            if dp_table[i - 1, j] > dp_table[i, j - 1]
                i -= 1
            else
                j -= 1
            end
        end
    end

    # Return the LCS
    return lcs
end

# Test the LCS function
str1 = "ABCDGH"
str2 = "AEDFHR"
lcs_length = lcs(str1, str2)
lcs_string = reconstruct_lcs(str1, str2, lcs_table)

# Print the LCS length and string
println("LCS Length:", lcs_length)
println("LCS String:", lcs_string)

```

Explanation:

This Julia code calculates the length and reconstructs the longest common subsequence (LCS) of two given strings using dynamic programming.

The lcs function initializes a matrix, dp_table, to store the lengths of the LCSs of the prefixes of the two strings. It iterates through the characters of the first string and for each character, it iterates through the characters of the second string. If the current characters are equal, the LCS length is one greater than the LCS length of the previous characters. Otherwise, the LCS length is the maximum of the LCS lengths of the previous characters.

The reconstruct_lcs function reconstructs the LCS from the dp_table. It starts from the bottom-right corner of the DP table and moves diagonally up until it reaches the top-left corner. If the current characters are equal, they are added to the LCS and the pointers are moved diagonally up. If the current characters are not equal, the pointer is moved up or left, depending on which has a greater LCS length.

Finally, the code tests the lcs and reconstruct_lcs functions with two input strings and prints the LCS length and string.