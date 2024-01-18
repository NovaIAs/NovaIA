```java
import java.util.*;
import java.io.*;
import java.math.*;
import java.text.*;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(System.out));
        StringTokenizer st = new StringTokenizer(br.readLine());

        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        int[][] graph = new int[n + 1][n + 1];

        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                graph[i][j] = Integer.MAX_VALUE;
            }
        }

        for (int i = 0; i < m; i++) {
            st = new StringTokenizer(br.readLine());

            int u = Integer.parseInt(st.nextToken());
            int v = Integer.parseInt(st.nextToken());
            int w = Integer.parseInt(st.nextToken());

            graph[u][v] = Math.min(graph[u][v], w);
        }

        for (int k = 1; k <= n; k++) {
            for (int i = 1; i <= n; i++) {
                for (int j = 1; j <= n; j++) {
                    if (graph[i][k] != Integer.MAX_VALUE && graph[k][j] != Integer.MAX_VALUE) {
                        graph[i][j] = Math.min(graph[i][j], graph[i][k] + graph[k][j]);
                    }
                }
            }
        }

        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                if (graph[i][j] == Integer.MAX_VALUE) {
                    bw.write("INF ");
                } else {
                    bw.write(graph[i][j] + " ");
                }
            }
            bw.newLine();
        }

        bw.flush();
        bw.close();
        br.close();
    }
}
```

Explanation:

This Java program implements the Floyd-Warshall algorithm to find the shortest path between all pairs of vertices in a weighted graph.

The program first reads the input, which consists of the number of vertices `n`, the number of edges `m`, and the edges of the graph.

The program then initializes the graph as a 2D array of integers, where each element `graph[i][j]` represents the weight of the edge from vertex `i` to vertex `j`. If there is no edge between two vertices, the weight is set to `Integer.MAX_VALUE`, which represents infinity.

The program then iterates over all possible pairs of vertices `(i, j)` and calculates the shortest path between them using the Floyd-Warshall algorithm.

The Floyd-Warshall algorithm works by considering all possible intermediate vertices `k` and checking if the path from vertex `i` to vertex `j` through vertex `k` is shorter than the current shortest path. If it is, the shortest path is updated.

The program finally prints the shortest paths between all pairs of vertices in the graph. If there is no path between two vertices, the program prints "INF".