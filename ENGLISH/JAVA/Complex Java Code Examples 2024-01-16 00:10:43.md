```java
import java.util.*;
import java.lang.*;

public class ComplexJavaCode {

    // This method takes a string and returns a map of the unique characters in the string and their frequencies.
    public static Map<Character, Integer> getCharacterFrequencies(String str) {
        Map<Character, Integer> charFrequencies = new HashMap<>();
        for (char c : str.toCharArray()) {
            charFrequencies.put(c, charFrequencies.getOrDefault(c, 0) + 1);
        }
        return charFrequencies;
    }

    // This method takes a list of integers and returns the maximum sum of any contiguous subarray in the list.
    public static int maxSubarraySum(List<Integer> nums) {
        int maxSum = Integer.MIN_VALUE;
        int currentSum = 0;
        for (int num : nums) {
            currentSum = Math.max(num, currentSum + num);
            maxSum = Math.max(maxSum, currentSum);
        }
        return maxSum;
    }

    // This method takes a binary tree and returns the maximum depth of the tree.
    public static int maxDepth(TreeNode root) {
        if (root == null) {
            return 0;
        }
        int leftDepth = maxDepth(root.left);
        int rightDepth = maxDepth(root.right);
        return Math.max(leftDepth, rightDepth) + 1;
    }

    // This method takes a graph represented as an adjacency list and returns the shortest path from a source vertex to all other vertices in the graph.
    public static Map<Vertex, Integer> shortestPath(Graph graph, Vertex source) {
        Map<Vertex, Integer> distances = new HashMap<>();
        for (Vertex vertex : graph.getVertices()) {
            distances.put(vertex, Integer.MAX_VALUE);
        }
        distances.put(source, 0);
        Queue<Vertex> queue = new LinkedList<>();
        queue.add(source);
        while (!queue.isEmpty()) {
            Vertex currentVertex = queue.poll();
            for (Edge edge : currentVertex.getEdges()) {
                Vertex adjacentVertex = edge.getDestination();
                int newDistance = distances.get(currentVertex) + edge.getWeight();
                if (newDistance < distances.get(adjacentVertex)) {
                    distances.put(adjacentVertex, newDistance);
                    queue.add(adjacentVertex);
                }
            }
        }
        return distances;
    }

    // This method takes a string and returns the longest palindromic substring in the string.
    public static String longestPalindromicSubstring(String str) {
        int maxLength = 0;
        int startIndex = 0;
        int endIndex = 0;
        for (int i = 0; i < str.length(); i++) {
            int left = i - 1;
            int right = i + 1;
            while (left >= 0 && right < str.length() && str.charAt(left) == str.charAt(right)) {
                left--;
                right++;
            }
            int currentLength = right - left - 1;
            if (currentLength > maxLength) {
                maxLength = currentLength;
                startIndex = left + 1;
                endIndex = right - 1;
            }

            left = i;
            right = i + 1;
            while (left >= 0 && right < str.length() && str.charAt(left) == str.charAt(right)) {
                left--;
                right++;
            }
            currentLength = right - left - 1;
            if (currentLength > maxLength) {
                maxLength = currentLength;
                startIndex = left + 1;
                endIndex = right - 1;
            }
        }
        return str.substring(startIndex, endIndex + 1);
    }

    public static void main(String[] args) {
        // Example usage of the methods
        String str = "Hello world!";
        Map<Character, Integer> charFrequencies = getCharacterFrequencies(str);
        System.out.println(charFrequencies); // {H=1, e=1, l=3, o=2, w=1, r=1, d=1}

        List<Integer> nums = Arrays.asList(1, 2, -3, 4, -1, 2, 1, -5, 4);
        int maxSum = maxSubarraySum(nums);
        System.out.println(maxSum); // 6

        TreeNode root = new TreeNode(1);
        root.left = new TreeNode(2);
        root.right = new TreeNode(3);
        root.left.left = new TreeNode(4);
        root.left.right = new TreeNode(5);
        int maxDepth = maxDepth(root);
        System.out.println(maxDepth); // 3

        Graph graph = new Graph();
        Vertex vertex1 = new Vertex("A");
        Vertex vertex2 = new Vertex("B");
        Vertex vertex3 = new Vertex("C");
        Vertex vertex4 = new Vertex("D");
        graph.addEdge(vertex1, vertex2, 1);
        graph.addEdge(vertex1, vertex3, 4);
        graph.addEdge(vertex2, vertex3, 2);
        graph.addEdge(vertex2, vertex4, 6);
        graph.addEdge(vertex3, vertex4,