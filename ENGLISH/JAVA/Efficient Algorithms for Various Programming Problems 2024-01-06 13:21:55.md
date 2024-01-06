```java
import java.util.*;
import java.io.*;
import java.math.*;

public class HighlyComplexCode {

    // Function to find the maximum sum of a contiguous subarray in a given array
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

    // Function to find the longest common substring between two strings
    public static String longestCommonSubstring(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];
        int maxLen = 0;
        int lastIndex = -1;

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                    if (dp[i][j] > maxLen) {
                        maxLen = dp[i][j];
                        lastIndex = i;
                    }
                }
            }
        }

        return str1.substring(lastIndex - maxLen, lastIndex);
    }

    // Function to find the minimum number of coins needed to make change for a given amount
    public static int minCoins(int[] coins, int amount) {
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

    // Function to find the number of ways to climb a staircase of n steps
    public static int climbStairs(int n) {
        int[] dp = new int[n + 1];

        dp[0] = 1;
        dp[1] = 1;

        for (int i = 2; i <= n; i++) {
            dp[i] = dp[i - 1] + dp[i - 2];
        }

        return dp[n];
    }

    // Function to find the maximum sum of non-adjacent elements in a given array
    public static int maxNonAdjacentSum(int[] arr) {
        int incl = arr[0];
        int excl = 0;

        for (int i = 1; i < arr.length; i++) {
            int newIncl = excl + arr[i];
            int newExcl = Math.max(incl, excl);

            incl = newIncl;
            excl = newExcl;
        }

        return Math.max(incl, excl);
    }

    // Function to find the longest increasing subsequence in a given array
    public static int longestIncreasingSubsequence(int[] arr) {
        int[] dp = new int[arr.length];

        Arrays.fill(dp, 1);

        for (int i = 1; i < arr.length; i++) {
            for (int j = 0; j < i; j++) {
                if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
                    dp[i] = dp[j] + 1;
                }
            }
        }

        int maxLen = 0;
        for (int i = 0; i < arr.length; i++) {
            maxLen = Math.max(maxLen, dp[i]);
        }

        return maxLen;
    }

    // Function to find the minimum number of deletions and insertions needed to transform one string into another
    public static int minDistance(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];

        for (int i = 0; i <= str1.length(); i++) {
            dp[i][0] = i;
        }

        for (int j = 0; j <= str2.length(); j++) {
            dp[0][j] = j;
        }

        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    dp[i][j] = Math.min(dp[i - 1][j], dp[i][j - 1]) + 1;
                }
            }
        }

        return dp[str1.length()][str2.length()];
    }

    // Function to find the maximum path sum in a binary tree
    public static int maxPathSum(TreeNode root) {
        int[] maxSum = new int[1];
        maxSum[0] = Integer.MIN_VALUE;

        maxPathSumUtil(root, maxSum);
        return maxSum[0];
    }

    private static int maxPathSumUtil(TreeNode root, int[] maxSum) {
        if (root == null) {
            return 0;
        }

        int leftSum = maxPathSumUtil(root.left, maxSum);
        int rightSum = maxPathSumUtil(root.right, maxSum);

        int maxSinglePathSum = Math.max(Math.max(leftSum, rightSum) + root.val, root.val);
        int maxTopDownPathSum = Math.max(maxSinglePathSum, leftSum + rightSum + root.val);

        maxSum[0] = Math.max(maxSum[0], maxTopDownPathSum);
        return maxSinglePathSum;
    }

    // Function to find the minimum number of jumps to reach the end of an array
    public static int minJumps(int[] arr) {
        int[] dp = new int[arr.length];

        Arrays.fill(dp, Integer.MAX_VALUE);
        dp[0] = 0;

        for (int i = 1; i < arr.length; i++) {
            for (int j = 0; j < i; j++) {
                if (dp[j] != Integer.MAX_VALUE && arr[j] + j >= i) {
                    dp[i] = Math.min(dp[i], dp[j] + 1);
                }
            }
        }

        return dp[arr.length - 1] == Integer.MAX_VALUE ? -1 : dp[arr.length - 1];
    }

    public static void main(String[] args) {
        // Example usage of the functions
        int[] arr = {1, -2, 3, 4, -1, 2, 1, -5, 4};
        System.out.println("Maximum Subarray Sum: " + maxSubArraySum(arr));

        String str1 = "ABCDGH";
        String str2 = "AEDFHR";
        System.out.println("Longest Common Substring: " + longestCommonSubstring(str1, str2));

        int[] coins = {1, 5, 10, 25};
        int amount = 30;
        System.out.println("Minimum Number of Coins: " + minCoins(coins, amount));

        int n = 4;
        System.out.println("Number of Ways to Climb Stairs: " + climbStairs(n));

        arr = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        System.out.println("Maximum Sum of Non-Adjacent Elements: " + maxNonAdjacentSum(arr));

        arr = new int[]{10, 2