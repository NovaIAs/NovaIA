```java
import java.util.*;
import java.lang.*;

class ComplexCode {

    // Function to find the maximum sum of a subsequence in an array
    static int maxSubsequenceSum(int[] arr) {
        int maxSoFar = 0;
        int maxEndingHere = 0;

        for (int i = 0; i < arr.length; i++) {
            maxEndingHere = maxEndingHere + arr[i];
            if (maxSoFar < maxEndingHere) {
                maxSoFar = maxEndingHere;
            }
            if (maxEndingHere < 0) {
                maxEndingHere = 0;
            }
        }

        return maxSoFar;
    }

    // Function to find the longest common subsequence between two strings
    static String longestCommonSubsequence(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }

        int i = str1.length();
        int j = str2.length();
        StringBuilder sb = new StringBuilder();

        while (i > 0 && j > 0) {
            if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                sb.append(str1.charAt(i - 1));
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

    // Function to find the minimum number of coins needed to make change for a given amount
    static int minCoinsForChange(int[] coins, int amount) {
        int[] dp = new int[amount + 1];

        Arrays.fill(dp, Integer.MAX_VALUE);
        dp[0] = 0;

        for (int i = 1; i <= amount; i++) {
            for (int j = 0; j < coins.length; j++) {
                if (i - coins[j] >= 0 && dp[i - coins[j]] != Integer.MAX_VALUE) {
                    dp[i] = Math.min(dp[i], dp[i - coins[j]] + 1);
                }
            }
        }

        return dp[amount] == Integer.MAX_VALUE ? -1 : dp[amount];
    }

    // Function to find the longest increasing subsequence in an array
    static int longestIncreasingSubsequence(int[] arr) {
        int[] dp = new int[arr.length];

        Arrays.fill(dp, 1);

        for (int i = 1; i < arr.length; i++) {
            for (int j = 0; j < i; j++) {
                if (arr[i] > arr[j]) {
                    dp[i] = Math.max(dp[i], dp[j] + 1);
                }
            }
        }

        int max = 0;

        for (int i = 0; i < arr.length; i++) {
            max = Math.max(max, dp[i]);
        }

        return max;
    }

    // Function to find the minimum number of deletions and insertions needed to transform one string into another
    static int minDeletionsAndInsertions(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }

        int deletions = str1.length() - dp[str1.length()][str2.length()];
        int insertions = str2.length() - dp[str1.length()][str2.length()];

        return deletions + insertions;
    }

    // Function to find the maximum sum of a non-empty subset of an array
    static int maxSubsetSum(int[] arr) {
        int maxSoFar = 0;
        int maxEndingHere = 0;

        for (int i = 0; i < arr.length; i++) {
            maxEndingHere = Math.max(maxEndingHere + arr[i], 0);
            maxSoFar = Math.max(maxSoFar, maxEndingHere);
        }

        return maxSoFar;
    }

    // Function to find the maximum sum of a non-empty contiguous subsequence of an array
    static int maxSubarraySum(int[] arr) {
        int maxSoFar = Integer.MIN_VALUE;
        int maxEndingHere = 0;

        for (int i = 0; i < arr.length; i++) {
            maxEndingHere = maxEndingHere + arr[i];
            if (maxSoFar < maxEndingHere) {
                maxSoFar = maxEndingHere;
            }
            if (maxEndingHere < 0) {
                maxEndingHere = 0;
            }
        }

        return maxSoFar;
    }

    // Function to find the maximum sum of a non-empty contiguous subsequence of an array, with at most k elements
    static int maxSubarraySumWithKElements(int[] arr, int k) {
        int maxSoFar = Integer.MIN_VALUE;
        int maxEndingHere = 0;
        int windowStart = 0;
        int windowEnd = 0;

        while (windowEnd < arr.length) {
            maxEndingHere = maxEndingHere + arr[windowEnd];

            if (windowEnd - windowStart + 1 == k) {
                maxSoFar = Math.max(maxSoFar, maxEndingHere);
                maxEndingHere = maxEndingHere - arr[windowStart];
                windowStart++;
            }

            windowEnd++;
        }

        return maxSoFar;
    }

    // Function to find the minimum number of deletions and insertions needed to transform one string into another, with at most k operations
    static int minDeletionsAndInsertionsWithKOperations(String str1, String str2, int k) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }

        int deletions = str1.length() - dp[str1.length()][str2.length()];
        int insertions = str2.length() - dp[str1.length()][str2.length()];

        if (deletions + insertions <= k) {
            return deletions + insertions;
        } else {
            return -1;
        }
    }

    // Function to find the maximum sum of a non-empty contiguous subsequence of an array, with at most k elements, and at least one element
    static int maxSubarraySumWithKElementsAndAtLeastOneElement(int[] arr, int k) {
        int maxSoFar = Integer.MIN_VALUE;
        int maxEndingHere = 0;
        int windowStart = 0;
        int windowEnd = 0;

        while (windowEnd < arr.length) {
            maxEndingHere = maxEndingHere + arr[windowEnd];

            if (windowEnd -