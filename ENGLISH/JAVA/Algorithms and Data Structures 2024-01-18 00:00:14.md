```java
import java.util.*;
import java.lang.*;
import java.math.BigInteger;

public class LargeAndDifferentiatedCode {

    // Helper class for finding the median of a set of numbers
    private static class MedianFinder {

        private PriorityQueue<Integer> minHeap; // Min heap for storing the larger half of the numbers
        private PriorityQueue<Integer> maxHeap; // Max heap for storing the smaller half of the numbers

        public MedianFinder() {
            minHeap = new PriorityQueue<>();
            maxHeap = new PriorityQueue<>(Collections.reverseOrder());
        }

        // Adds a number to the set
        public void add(int num) {
            // If the max heap is empty or the number is greater than the max heap's root, add the number to the min heap
            if (maxHeap.isEmpty() || num > maxHeap.peek()) {
                minHeap.add(num);
            } else { // Otherwise, add the number to the max heap
                maxHeap.add(num);
            }

            // Balance the heaps to ensure the median is correctly calculated
            balanceHeaps();
        }

        // Returns the median of the set
        public double getMedian() {
            // If the heaps have equal sizes, the median is the average of their roots
            if (minHeap.size() == maxHeap.size()) {
                return (double) (minHeap.peek() + maxHeap.peek()) / 2;
            } else if (minHeap.size() > maxHeap.size()) { // If the min heap has more elements, the median is its root
                return minHeap.peek();
            } else { // Otherwise, the median is the root of the max heap
                return maxHeap.peek();
            }
        }

        // Balances the heaps to ensure the median is correctly calculated
        private void balanceHeaps() {
            // If the min heap has more than one more element than the max heap, move the root of the min heap to the max heap
            while (minHeap.size() - maxHeap.size() > 1) {
                maxHeap.add(minHeap.poll());
            }

            // If the max heap has more than one more element than the min heap, move the root of the max heap to the min heap
            while (maxHeap.size() - minHeap.size() > 1) {
                minHeap.add(maxHeap.poll());
            }
        }
    }

    // Calculates the kth largest element in an array in O(n) time using a min heap
    public static int findKthLargest(int[] arr, int k) {
        // Create a min heap
        PriorityQueue<Integer> minHeap = new PriorityQueue<>();

        // Add the first k elements of the array to the heap
        for (int i = 0; i < k; i++) {
            minHeap.add(arr[i]);
        }

        // Iterate over the remaining elements of the array
        for (int i = k; i < arr.length; i++) {
            // If the current element is greater than the root of the heap, replace the root with the current element
            if (arr[i] > minHeap.peek()) {
                minHeap.poll();
                minHeap.add(arr[i]);
            }
        }

        // The root of the heap is the kth largest element
        return minHeap.peek();
    }

    // Calculates the longest common subsequence of two strings in O(m*n) time using dynamic programming
    public static String longestCommonSubsequence(String str1, String str2) {
        // Create a 2D array to store the length of the longest common subsequence of substrings of str1 and str2
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];

        // Calculate the length of the longest common subsequence
        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }

        // Construct the longest common subsequence
        StringBuilder sb = new StringBuilder();
        int i = str1.length();
        int j = str2.length();
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

    // Finds all permutations of a string in O(n!) time using a recursive backtracking algorithm
    public static List<String> permutations(String str) {
        List<String> permutations = new ArrayList<>();

        // Base case: empty string has only one permutation (itself)
        if (str.isEmpty()) {
            permutations.add("");
            return permutations;
        }

        // Recursive case: for each character in the string, generate all permutations of the remaining characters
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            String remaining = str.substring(0, i) + str.substring(i + 1);
            List<String> subPermutations = permutations(remaining);

            // Add the current character to the beginning of each subpermutation to generate all permutations of the original string
            for (String subPermutation : subPermutations) {
                permutations.add(c + subPermutation);
            }
        }

        return permutations;
    }

    // Finds all subsets of a set in O(2^n) time using a recursive backtracking algorithm
    public static List<List<Integer>> subsets(int[] nums) {
        List<List<Integer>> subsets = new ArrayList<>();

        // Base case: empty set has only one subset (itself)
        if (nums.length == 0) {
            subsets.add(new ArrayList<>());
            return subsets;
        }

        // Recursive case: for each element in the set, generate all subsets of the remaining elements
        for (int i = 0; i < nums.length; i++) {
            int current = nums[i];
            List<List<Integer>> subsubsets = subsets(Arrays.copyOfRange(nums, i + 1, nums.length));

            // Add the current element to the beginning of each subsubset to generate all subsets of the original set
            for (List<Integer> subsubset : subsubsets) {
                List<Integer> subset = new ArrayList<>(subsubset);
                subset.add(0, current);
                subsets.add(subset);
            }

            // Add the subsubsets without the current element to the original set to generate all subsets of the original set
            subsets.addAll(subsubsets);
        }

        return subsets;
    }

    // Calculates the factorial of a number in O(n) time using a recursive algorithm
    public static BigInteger factorial