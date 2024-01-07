```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <string>
#include <sstream>
#include <cmath>
#include <limits>
#include <iomanip>

using namespace std;

#define FOR(i, a, b) for (int i = (a); i < (b); ++i)
#define REP(i, n) FOR(i, 0, n)

typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<pii> vii;
typedef vector<vi> vvi;
typedef map<int, int> mii;
typedef set<int> si;
typedef queue<int> qi;
typedef stack<int> si;

// Function to find the longest common substring of two strings
string longestCommonSubstring(string str1, string str2) {
    // Create a matrix to store the lengths of the longest common substrings of all substrings of str1 and str2
    int m = str1.size();
    int n = str2.size();
    int** dp = new int*[m + 1];
    FOR(i, 0, m + 1) {
        dp[i] = new int[n + 1];
    }

    // Initialize the first row and column of the matrix to 0
    FOR(i, 0, m + 1) {
        dp[i][0] = 0;
    }
    FOR(j, 0, n + 1) {
        dp[0][j] = 0;
    }

    // Fill the matrix by comparing the characters of str1 and str2
    FOR(i, 1, m + 1) {
        FOR(j, 1, n + 1) {
            if (str1[i - 1] == str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = 0;
            }
        }
    }

    // Find the maximum value in the matrix
    int max_len = 0;
    int max_i = 0;
    int max_j = 0;
    FOR(i, 1, m + 1) {
        FOR(j, 1, n + 1) {
            if (dp[i][j] > max_len) {
                max_len = dp[i][j];
                max_i = i;
                max_j = j;
            }
        }
    }

    // Create a string to store the longest common substring
    string lcs = "";
    while (max_len > 0) {
        lcs = str1[max_i - 1] + lcs;
        max_i--;
        max_len--;
    }

    // Free the memory allocated for the matrix
    FOR(i, 0, m + 1) {
        delete[] dp[i];
    }
    delete[] dp;

    return lcs;
}

// Function to find the longest palindrome substring of a string
string longestPalindromeSubstring(string str) {
    // Create a matrix to store the lengths of the longest palindromes of all substrings of str
    int n = str.size();
    int** dp = new int*[n];
    FOR(i, 0, n) {
        dp[i] = new int[n];
    }

    // Initialize the diagonal of the matrix to 1
    FOR(i, 0, n) {
        dp[i][i] = 1;
    }

    // Fill the matrix by comparing the characters of str
    for (int len = 2; len <= n; len++) {
        for (int i = 0; i <= n - len; i++) {
            int j = i + len - 1;
            if (len == 2) {
                dp[i][j] = (str[i] == str[j]);
            } else {
                dp[i][j] = (str[i] == str[j] && dp[i + 1][j - 1]);
            }
        }
    }

    // Find the maximum value in the matrix
    int max_len = 0;
    int max_i = 0;
    int max_j = 0;
    FOR(i, 0, n) {
        FOR(j, 0, n) {
            if (dp[i][j] > max_len) {
                max_len = dp[i][j];
                max_i = i;
                max_j = j;
            }
        }
    }

    // Create a string to store the longest palindrome substring
    string lps = "";
    while (max_len > 0) {
        lps = str[max_i] + lps;
        max_i++;
        max_len--;
    }

    // Free the memory allocated for the matrix
    FOR(i, 0, n) {
        delete[] dp[i];
    }
    delete[] dp;

    return lps;
}

// Function to find the minimum number of edits to transform one string into another
int minEditDistance(string str1, string str2) {
    // Create a matrix to store the number of edits required to transform one string into another
    int m = str1.size();
    int n = str2.size();
    int** dp = new int*[m + 1];
    FOR(i, 0, m + 1) {
        dp[i] = new int[n + 1];
    }

    // Initialize the first row and column of the matrix to the lengths of str1 and str2, respectively
    FOR(i, 0, m + 1) {
        dp[i][0] = i;
    }
    FOR(j, 0, n + 1) {
        dp[0][j] = j;
    }

    // Fill the matrix by comparing the characters of str1 and str2
    FOR(i, 1, m + 1) {
        FOR(j, 1, n + 1) {
            if (str1[i - 1] == str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = min({dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1]}) + 1;
            }
        }
    }

    // The minimum number of edits required to transform str1 into str2 is the value in the bottom-right corner of the matrix
    int min_edits = dp[m][n];

    // Free the memory allocated for the matrix
    FOR(i, 0, m + 1) {
        delete[] dp[i];
    }
    delete[] dp;

    return min_edits;
}

// Function to find the longest increasing subsequence of a given array
vi longestIncreasingSubsequence(vi arr) {
    // Create a vector to store the lengths of the longest increasing subsequences ending at each index
    int n = arr.size();
    vi dp(n, 1);

    // Create a vector to store the predecessors of each element in the longest increasing subsequence
    vi prev(n, -1);

    // Find the longest increasing subsequence
    int max_len = 1;
    int max_idx = 0;
    FOR(i, 1, n) {
        FOR(j, 0, i) {
            if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1;
                prev[i] = j;
                if (dp[i] > max_len) {
                    max_len = dp[i];
                    max_idx = i;
                }
            }
        }
    }

    // Create a vector to store the longest increasing subsequence
    vi lis;
    while (max_idx != -1) {
        lis.push_back(arr[max_idx]);
        max_idx = prev[max_idx];
    }

    // Reverse the vector to get the longest increasing subsequence in ascending order
    reverse(lis.begin(), lis.end());

    return lis;
}

// Function to find the shortest common supersequence of two strings
string shortestCommonSupersequence(string str1, string str2) {
    