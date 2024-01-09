```java
import java.util.*;
import java.io.*;

public class ComplexJavaCode {

    // This method finds the longest common substring between two strings.
    public static String longestCommonSubstring(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];
        int maxLen = 0;
        int maxI = 0;
        int maxJ = 0;
        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                    if (dp[i][j] > maxLen) {
                        maxLen = dp[i][j];
                        maxI = i;
                        maxJ = j;
                    }
                }
            }
        }
        return str1.substring(maxI - maxLen, maxI);
    }

    // This method finds the shortest path between two nodes in a graph.
    public static List<Integer> shortestPath(Graph graph, int src, int dest) {
        Queue<Integer> queue = new LinkedList<>();
        queue.add(src);
        Map<Integer, Integer> parent = new HashMap<>();
        parent.put(src, null);
        while (!queue.isEmpty()) {
            int current = queue.poll();
            if (current == dest) {
                break;
            }
            for (int neighbor : graph.getNeighbors(current)) {
                if (!parent.containsKey(neighbor)) {
                    queue.add(neighbor);
                    parent.put(neighbor, current);
                }
            }
        }
        List<Integer> path = new ArrayList<>();
        int current = dest;
        while (current != null) {
            path.add(current);
            current = parent.get(current);
        }
        Collections.reverse(path);
        return path;
    }

    // This method finds the minimum spanning tree of a graph.
    public static Graph minimumSpanningTree(Graph graph) {
        Set<Integer> visited = new HashSet<>();
        Graph mst = new Graph();
        PriorityQueue<Edge> edges = new PriorityQueue<>((a, b) -> a.weight - b.weight);
        edges.addAll(graph.getEdges());
        while (!edges.isEmpty()) {
            Edge edge = edges.poll();
            int src = edge.src;
            int dest = edge.dest;
            if (!visited.contains(src) || !visited.contains(dest)) {
                mst.addEdge(src, dest, edge.weight);
                visited.add(src);
                visited.add(dest);
                for (Edge neighbor : graph.getEdges(src)) {
                    if (!visited.contains(neighbor.dest)) {
                        edges.add(neighbor);
                    }
                }
                for (Edge neighbor : graph.getEdges(dest)) {
                    if (!visited.contains(neighbor.dest)) {
                        edges.add(neighbor);
                    }
                }
            }
        }
        return mst;
    }

    // This method finds the maximum independent set of a graph.
    public static Set<Integer> maximumIndependentSet(Graph graph) {
        Set<Integer> visited = new HashSet<>();
        Set<Integer> independentSet = new HashSet<>();
        for (int vertex : graph.getVertices()) {
            if (!visited.contains(vertex)) {
                Set<Integer> currentIndependentSet = new HashSet<>();
                dfs(graph, vertex, visited, currentIndependentSet);
                if (currentIndependentSet.size() > independentSet.size()) {
                    independentSet = currentIndependentSet;
                }
            }
        }
        return independentSet;
    }

    private static void dfs(Graph graph, int vertex, Set<Integer> visited, Set<Integer> currentIndependentSet) {
        visited.add(vertex);
        currentIndependentSet.add(vertex);
        for (int neighbor : graph.getNeighbors(vertex)) {
            if (!visited.contains(neighbor)) {
                dfs(graph, neighbor, visited, currentIndependentSet);
            }
        }
    }

    public static void main(String[] args) {
        // Example usage of the methods.
        String str1 = "ABCD