```c#
// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;

// Define the main class
public class AdvancedCSharpCode
{
    // Define a delegate for a function that takes an integer and returns an integer
    public delegate int Operation(int n);

    // Define a class to represent a node in a binary tree
    public class Node
    {
        public int Value { get; set; }
        public Node Left { get; set; }
        public Node Right { get; set; }

        public Node(int value)
        {
            Value = value;
        }
    }

    // Define a method to perform a breadth-first search on a binary tree
    public static IEnumerable<int> BreadthFirstSearch(Node root)
    {
        // Create a queue to store the nodes to be visited
        Queue<Node> queue = new Queue<Node>();

        // Add the root node to the queue
        queue.Enqueue(root);

        // While there are nodes in the queue
        while (queue.Count > 0)
        {
            // Dequeue the first node from the queue
            Node node = queue.Dequeue();

            // Yield the value of the node
            yield return node.Value;

            // If the node has a left child, add it to the queue
            if (node.Left != null)
            {
                queue.Enqueue(node.Left);
            }

            // If the node has a right child, add it to the queue
            if (node.Right != null)
            {
                queue.Enqueue(node.Right);
            }
        }
    }

    // Define a method to perform a depth-first search on a binary tree
    public static IEnumerable<int> DepthFirstSearch(Node root)
    {
        // If the root node is null, return an empty sequence
        if (root == null)
        {
            return Enumerable.Empty<int>();
        }

        // Yield the value of the root node
        yield return root.Value;

        // Perform a depth-first search on the left subtree
        foreach (int value in DepthFirstSearch(root.Left))
        {
            yield return value;
        }

        // Perform a depth-first search on the right subtree
        foreach (int value in DepthFirstSearch(root.Right))
        {
            yield return value;
        }
    }

    // Define a method to find the maximum sum of a contiguous subarray in an array of integers
    public static int MaximumSubarraySum(int[] array)
    {
        // Initialize the current and maximum sums to 0
        int currentSum = 0;
        int maximumSum = 0;

        // For each element in the array
        foreach (int element in array)
        {
            // Update the current sum by adding the current element
            currentSum += element;

            // If the current sum is greater than the maximum sum, update the maximum sum
            if (currentSum > maximumSum)
            {
                maximumSum = currentSum;
            }

            // If the current sum is negative, reset it to 0
            if (currentSum < 0)
            {
                currentSum = 0;
            }
        }

        // Return the maximum sum
        return maximumSum;
    }

    // Define a method to find the longest common substring between two strings
    public static string LongestCommonSubstring(string str1, string str2)
    {
        // Create a matrix to store the lengths of the longest common substrings
        int[,] matrix = new int[str1.Length + 1, str2.Length + 1];

        // Initialize the first row and column of the matrix to 0
        for (int i = 0; i <= str1.Length; i++)
        {
            matrix[i, 0] = 0;
        }
        for (int j = 0; j <= str2.Length; j++)
        {
            matrix[0, j] = 0;
        }

        // For each character in the first string
        for (int i = 1; i <= str1.Length; i++)
        {
            // For each character in the second string
            for (int j = 1; j <= str2.Length; j++)
            {
                // If the characters match, increment the length of the longest common substring
                if (str1[i - 1] == str2[j - 1])
                {
                    matrix[i, j] = matrix[i - 1, j - 1] + 1;
                }
                // Otherwise, set the length of the longest common substring to 0
                else
                {
                    matrix[i, j] = 0;
                }
            }
        }

        // Find the maximum length of the longest common substring
        int maxLength = 0;
        for (int i = 0; i <= str1.Length; i++)
        {
            for (int j = 0; j <= str2.Length; j++)
            {
                if (matrix[i, j] > maxLength)
                {
                    maxLength = matrix[i, j];
                }
            }
        }

        // Find the starting index of the longest common substring in the first string
        int startIndex = 0;
        for (int i = 0; i <= str1.Length; i++)
        {
            for (int j = 0; j <= str2.Length; j++)
            {
                if (matrix[i, j] == maxLength)
                {
                    startIndex = i - maxLength;
                    break;
                }
            }
        }

        // Return the longest common substring
        return str1.Substring(startIndex, maxLength);
    }

    // Define a method to find the minimum number of coins needed to make change for a given amount of money
    public static int MinimumCoins(int amount, int[] coins)
    {
        // Create an array to store the minimum number of coins needed to make change for each amount up to the given amount
        int[] dp = new int[amount + 1];

        // Initialize the first element of the array to 0
        dp[0] = 0;

        // For each amount from 1 to the given amount
        for (int i = 1; i <= amount; i++)
        {
            // Initialize the minimum number of coins needed to make change for the current amount to infinity
            dp[i] = int.MaxValue;

            // For each coin denomination
            foreach (int coin in coins)
            {
                // If the coin denomination is less than or equal to the current amount
                if (coin <= i)
                {
                    // If the minimum number of coins needed to make change for the current amount minus the coin denomination is not infinity