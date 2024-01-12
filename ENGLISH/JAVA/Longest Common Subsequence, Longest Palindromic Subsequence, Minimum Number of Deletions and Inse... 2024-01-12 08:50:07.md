```java
import java.util.*;
import java.lang.*;

class Solution {

    // Function to find the longest common subsequence of two strings
    public String longestCommonSubsequence(String text1, String text2) {
        int m = text1.length();
        int n = text2.length();

        // Create a matrix to store the lengths of the longest common subsequences
        // of the prefixes of the two strings
        int[][] dp = new int[m + 1][n + 1];

        // Fill the matrix in bottom-up manner
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (text1.charAt(i - 1) == text2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }

        // Construct the longest common subsequence from the matrix
        StringBuilder sb = new StringBuilder();
        int i = m;
        int j = n;
        while (i > 0 && j > 0) {
            if (text1.charAt(i - 1) == text2.charAt(j - 1)) {
                sb.append(text1.charAt(i - 1));
                i--;
                j--;
            } else {
                if (dp[i - 1][j] > dp[i][j - 1]) {
                    i--;
                } else {
                    j--;
                }
            }
        }

        return sb.reverse().toString();
    }

    // Function to find the longest palindromic subsequence of a string
    public String longestPalindromicSubsequence(String text) {
        int n = text.length();

        // Create a matrix to store the lengths of the longest palindromic subsequences
        // of the prefixes of the string
        int[][] dp = new int[n][n];

        // Fill the matrix in bottom-up manner
        for (int i = n - 1; i >= 0; i--) {
            dp[i][i] = 1;
            for (int j = i + 1; j < n; j++) {
                if (text.charAt(i) == text.charAt(j)) {
                    dp[i][j] = dp[i + 1][j - 1] + 2;
                } else {
                    dp[i][j] = Math.max(dp[i + 1][j], dp[i][j - 1]);
                }
            }
        }

        // Construct the longest palindromic subsequence from the matrix
        StringBuilder sb = new StringBuilder();
        int i = 0;
        int j = n - 1;
        while (i < j) {
            if (text.charAt(i) == text.charAt(j)) {
                sb.append(text.charAt(i));
                i++;
                j--;
            } else {
                if (dp[i + 1][j] > dp[i][j - 1]) {
                    i++;
                } else {
                    j--;
                }
            }
        }

        if (i == j) {
            sb.append(text.charAt(i));
        }

        return sb.toString();
    }

    // Function to find the minimum number of deletions and insertions required to
    // transform one string into another
    public int minDistance(String text1, String text2) {
        int m = text1.length();
        int n = text2.length();

        // Create a matrix to store the minimum number of deletions and insertions
        // required to transform the prefixes of the two strings into each other
        int[][] dp = new int[m + 1][n + 1];

        // Fill the matrix in bottom-up manner
        for (int i = 1; i <= m; i++) {
            dp[i][0] = i;
        }
        for (int j = 1; j <= n; j++) {
            dp[0][j] = j;
        }
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (text1.charAt(i - 1) == text2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    dp[i][j] = Math.min(dp[i - 1][j], dp[i][j - 1]) + 1;
                }
            }
        }

        // The minimum number of deletions and insertions required to transform one
        // string into another is the value in the bottom-right corner of the matrix
        return dp[m][n];
    }

    // Function to find the longest common substring of two strings
    public String longestCommonSubstring(String text1, String text2) {
        int m = text1.length();
        int n = text2.length();

        // Create a matrix to store the lengths of the longest common substrings
        // of the prefixes of the two strings
        int[][] dp = new int[m + 1][n + 1];

        // Fill the matrix in bottom-up manner
        int maxLen = 0;
        int maxI = 0;
        int maxJ = 0;
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (text1.charAt(i - 1) == text2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                    if (dp[i][j] > maxLen) {
                        maxLen = dp[i][j];
                        maxI = i;
                        maxJ = j;
                    }
                }
            }
        }

        // Construct the longest common substring from the matrix
        StringBuilder sb = new StringBuilder();
        while (dp[maxI][maxJ] > 0) {
            sb.append(text1.charAt(maxI - 1));
            maxI--;
            maxJ--;
        }

        return sb.reverse().toString();
    }

    // Function to find the longest repeating substring of a string
    public String longestRepeatedSubstring(String text) {
        int n = text.length();

        // Create a suffix array to store the starting positions of the suffixes of the string
        int[] suffixArray = new int[n];
        suffixArray[n - 1] = n;
        for (int i = n - 2; i >= 0; i--) {
            if (text.charAt(i) == text.charAt(i + 1)) {
                suffixArray[i] = suffixArray[i + 1] - 1;
            } else {
                suffixArray[i] = i + 1;
            }
        }

        // Create a longest common prefix array to store the lengths of the longest common prefixes
        // of the suffixes of the string
        int[] lcpArray = new int[n];
        lcpArray[0] = 0;
        int k = 0;
        for (int i = 1; i < n; i++) {
            int j = suffixArray[i];
            while (text.charAt(i + k) == text.charAt(j + k)) {
                k++;
            }
            lcpArray[i] = k;
            if (k > 0) {
                k--;
            }
        }

        // Find the longest repeating substring of the string
        int maxLen = 0;
        int maxI = 0;
        for (int i = 1; i < n; i++) {
            if (lcpArray[i] > maxLen) {
                maxLen = lcpArray[i];
                maxI = i;
            }
        }

        // Construct the longest repeating substring from the suffix array and the longest common prefix array
        StringBuilder sb = new StringBuilder();
        while (maxLen > 0) {
            sb.append(text.substring(suffixArray[maxI], suffixArray[maxI] + maxLen));
            maxI++;
            maxLen--;
        }

        return sb.toString();
    }

    // Function to find the longest increasing subsequence of an array of integers
    public int longestIncreasingSubsequence(int[] nums) {
        int n = nums.length;

        // Create a dp array to store the lengths