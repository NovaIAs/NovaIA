```c++
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <functional>

using namespace std;

// Define a custom comparison function for sorting strings
bool compare_strings(const string& a, const string& b) {
    // Compare the lengths of the strings
    if (a.length() != b.length()) {
        return a.length() < b.length();
    }

    // If the lengths are equal, compare the strings lexicographically
    return a < b;
}

// Define a function to group strings by their length
map<int, vector<string>> group_strings_by_length(const vector<string>& strings) {
    // Create a map to store the groups of strings
    map<int, vector<string>> groups;

    // Iterate over the strings
    for (const string& str : strings) {
        // Get the length of the string
        int length = str.length();

        // Add the string to the corresponding group
        groups[length].push_back(str);
    }

    // Return the map of groups
    return groups;
}

// Define a function to find the longest common subsequence of two strings
string longest_common_subsequence(string a, string b) {
    // Create a matrix to store the lengths of the longest common subsequences of the prefixes of the two strings
    int lcs[a.length() + 1][b.length() + 1];

    // Initialize the first row and column of the matrix to 0
    for (int i = 0; i <= a.length(); i++) {
        lcs[i][0] = 0;
    }
    for (int j = 0; j <= b.length(); j++) {
        lcs[0][j] = 0;
    }

    // Iterate over the rows and columns of the matrix
    for (int i = 1; i <= a.length(); i++) {
        for (int j = 1; j <= b.length(); j++) {
            // If the characters at the current positions in the two strings are the same, the length of the longest common subsequence of the current prefixes is one more than the length of the longest common subsequence of the previous prefixes
            if (a[i - 1] == b[j - 1]) {
                lcs[i][j] = lcs[i - 1][j - 1] + 1;
            }
            // Otherwise, the length of the longest common subsequence of the current prefixes is the maximum of the lengths of the longest common subsequences of the prefixes ending at the previous positions in the two strings
            else {
                lcs[i][j] = max(lcs[i - 1][j], lcs[i][j - 1]);
            }
        }
    }

    // Reconstruct the longest common subsequence from the matrix
    string lcs_str;
    int i = a.length();
    int j = b.length();
    while (i > 0 && j > 0) {
        if (a[i - 1] == b[j - 1]) {
            lcs_str = a[i - 1] + lcs_str;
            i--;
            j--;
        } else if (lcs[i - 1][j] > lcs[i][j - 1]) {
            i--;
        } else {
            j--;
        }
    }

    // Return the longest common subsequence
    return lcs_str;
}

// Define a function to find the most frequently occurring word in a string
string most_frequent_word(string str) {
    // Convert the string to lowercase and tokenize it by spaces
    transform(str.begin(), str.end(), str.begin(), ::tolower);
    vector<string> words;
    istringstream iss(str);
    string word;
    while (iss >> word) {
        words.push_back(word);
    }

    // Create a map to store the word frequencies
    map<string, int> word_freq;
    for (const string& word : words) {
        word_freq[word]++;
    }

    // Find the word with the highest frequency
    string most_frequent_word;
    int max_freq = 0;
    for (const pair<string, int>& word_freq_pair : word_freq) {
        if (word_freq_pair.second > max_freq) {
            most_frequent_word = word_freq_pair.first;
            max_freq = word_freq_pair.second;
        }
    }

    // Return the most frequently occurring word
    return most_frequent_word;
}

int main() {
    // Define a vector of strings
    vector<string> strings = {"apple", "banana", "cherry", "dog", "cat", "fish"};

    // Sort the strings by their length
    sort(strings.begin(), strings.end(), compare_strings);

    // Group the strings by their length
    map<int, vector<string>> groups = group_strings_by_length(strings);

    // Print the groups of strings
    for (const pair<int, vector<string>>& group : groups) {
        cout << "Length: " << group.first << endl;
        for (const string& str : group.second) {
            cout << "\t" << str << endl;
        }
    }

    // Compute the longest common subsequence of two strings
    string a = "ABCDGH";
    string b = "AEDFHR";
    string lcs = longest_common_subsequence(a, b);
    cout << "Longest common subsequence of \"" << a << "\" and \"" << b << "\": " << lcs << endl;

    // Find the most frequently occurring word in a string
    string str = "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog.";
    string most_freq_word = most_frequent_word(str);
    cout << "Most frequently occurring word in \"" << str << "\": " << most_freq_word << endl;

    return 0;
}
```
This code performs a variety of string manipulation tasks, including sorting, grouping, finding the longest common subsequence, and finding the most frequently occurring word in a string. The code is heavily commented to explain each section and function in detail. Here's a summary of what each part of the code does:

1. **String Sorting:**
   - It defines a custom comparison function `compare_strings` to sort strings by length.

2. **String Grouping:**
   - It defines a function `group_strings_by_length` to group strings based on their lengths.

3. **Longest Common Subsequence:**
   - It defines a function `longest_common_subsequence` to find the longest common subsequence of two strings using dynamic programming.

4. **Most Frequent Word:**
   - It defines a function `most_frequent_word` to find the most frequently occurring word in a string.

In the `main` function:

1. It creates a vector of strings `strings`.

2. It sorts the strings by length using `sort` and the custom comparison function.

3. It groups the strings by length using `group_strings_by_length` and prints the groups.

4. It computes the longest common subsequence of two strings "ABCDGH" and "AEDFHR".

5. It finds the most frequently occurring word in a given string.

This code showcases advanced string manipulation techniques and is designed to be a comprehensive demonstration of various string operations in C++.