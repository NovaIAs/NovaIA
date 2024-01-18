```java
import java.util.*;
import java.lang.*;

class ComplexCode {

    // Method to find the maximum sum of a subarray of a given array
    public static int maxSubArraySum(int[] arr) {
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

    // Method to find the longest common substring between two strings
    public static String longestCommonSubstring(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];
        int maxLen = 0;
        int end = 0;

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                    if (dp[i][j] > maxLen) {
                        maxLen = dp[i][j];
                        end = i;
                    }
                }
            }
        }

        return str1.substring(end - maxLen, end);
    }

    // Method to find the minimum number of coins needed to make a change for a given amount
    public static int minNumberOfCoins(int[] coins, int amount) {
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

    // Method to find the longest increasing subsequence of a given array
    public static int[] longestIncreasingSubsequence(int[] arr) {
        int[] dp = new int[arr.length];
        int[] prev = new int[arr.length];
        int maxLength = 0;
        int lastIndex = -1;

        for (int i = 0; i < arr.length; i++) {
            dp[i] = 1;
            prev[i] = -1;

            for (int j = 0; j < i; j++) {
                if (arr[i] > arr[j] && dp[j] + 1 > dp[i]) {
                    dp[i] = dp[j] + 1;
                    prev[i] = j;
                }
            }

            if (dp[i] > maxLength) {
                maxLength = dp[i];
                lastIndex = i;
            }
        }

        int[] lis = new int[maxLength];
        int index = lastIndex;
        for (int i = maxLength - 1; i >= 0; i--) {
            lis[i] = arr[index];
            index = prev[index];
        }

        return lis;
    }

    public static void main(String[] args) {
        // Example usage of the methods
        int[] arr = {1, 2, 3, 4, -5, 6, 7, 8, 9};
        System.out.println("Maximum sum of a subarray: " + maxSubArraySum(arr));

        String str1 = "ABCD