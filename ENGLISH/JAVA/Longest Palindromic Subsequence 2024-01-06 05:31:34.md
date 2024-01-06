```java
import java.util.*;
import java.lang.*;
import java.io.*;

class Solution {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = sc.nextInt();
        }

        int[][] dp = new int[n][n];
        for (int i = 0; i < n; i++) {
            dp[i][i] = 1;
        }

        for (int l = 2; l <= n; l++) {
            for (int i = 0; i < n - l + 1; i++) {
                int j = i + l - 1;
                if (l == 2) {
                    dp[i][j] = (arr[i] == arr[j]) ? 2 : 1;
                } else {
                    if (arr[i] == arr[j] && dp[i + 1][j - 1] == l - 1) {
                        dp[i][j] = l;
                    } else {
                        dp[i][j] = 1;
                    }
                }
            }
        }

        int maxLen = 0;
        int[] maxLenIndices = new int[2];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (dp[i][j] > maxLen) {
                    maxLen = dp[i][j];
                    maxLenIndices[0] = i;
                    maxLenIndices[1] = j;
                }
            }
        }

        System.out.println(maxLen);
        for (int i = maxLenIndices[0]; i <= maxLenIndices[1]; i++) {
            System.out.print(arr[i] + " ");
        }
    }
}
```

This code finds the longest palindromic subsequence in a given string.

**Explanation:**

* The code starts by creating a 2D array `dp` to store the length of the longest palindromic subsequence of each substring of the given string. The diagonal elements of `dp` are all initialized to 1, since a single character is always a palindrome.
* The code then uses a nested loop to fill in the remaining elements of `dp`. For each substring of length `l`, the code checks if the first and last characters of the substring are the same. If they are, and the substring of length `l-2` is also a palindrome, then the current substring is a palindrome of length `l`. Otherwise, the current substring is not a palindrome and its length is 1.
* The code then finds the longest palindromic subsequence of the given string by finding the maximum value in `dp`. The indices of the maximum value in `dp` indicate the start and end indices of the longest palindromic subsequence.
* Finally, the code prints the longest palindromic subsequence of the given string.