```java
import java.util.*;
import java.io.*;

class Solution {
    static int[] dx = {-1, 0, 1, 0};
    static int[] dy = {0, 1, 0, -1};

    static int n, m;
    static char[][] grid;
    static boolean[][] visited;
    static int[][] dist;
    static int[][] parent;

    static void bfs(int sx, int sy) {
        Queue<Integer> Q = new LinkedList<>();
        Q.add(sx);
        Q.add(sy);

        visited[sx][sy] = true;
        dist[sx][sy] = 0;
        parent[sx][sy] = -1;

        while (!Q.isEmpty()) {
            int x = Q.remove();
            int y = Q.remove();

            for (int i = 0; i < 4; i++) {
                int nx = x + dx[i];
                int ny = y + dy[i];

                if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#' && !visited[nx][ny]) {
                    visited[nx][ny] = true;
                    dist[nx][ny] = dist[x][y] + 1;
                    parent[nx][ny] = (x << 1) + i;

                    Q.add(nx);
                    Q.add(ny);
                }
            }
        }
    }

    static void printPath(int x, int y) {
        if (parent[x][y] == -1) {
            System.out.print("(" + x + ", " + y + ")");
        } else {
            int px = parent[x][y] >> 1;
            int py = parent[x][y] & 1;

            printPath(px, py);

            if (dx[py - px + 2] == 1) {
                System.out.print(" D");
            } else if (dx[py - px + 2] == -1) {
                System.out.print(" U");
            } else if (dy[py - px + 2] == 1) {
                System.out.print(" R");
            } else {
                System.out.print(" L");
            }

            System.out.print("(" + x + ", " + y + ")");
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(System.out));

        String[] tokens = br.readLine().split(" ");
        n = Integer.parseInt(tokens[0]);
        m = Integer.parseInt(tokens[1]);

        grid = new char[n][m];
        visited = new boolean[n][m];
        dist = new int[n][m];
        parent = new int[n][m];

        int sx = -1, sy = -1;
        int ex = -1, ey = -1;

        for (int i = 0; i < n; i++) {
            String line = br.readLine();

            for (int j = 0; j < m; j++) {
                grid[i][j] = line.charAt(j);

                if (grid[i][j] == 'S') {
                    sx = i;
                    sy = j;
                } else if (grid[i][j] == 'E') {
                    ex = i;
                    ey = j;
                }
            }
        }

        bfs(sx, sy);

        if (visited[ex][ey]) {
            bw.write("YES\n");
            bw.write(dist[ex][ey] + "\n");

            printPath(ex, ey);
            bw.write("\n");
        } else {
            bw.write("NO\n");
        }

        bw.flush();
        bw.close();
    }
}
```

This Java program solves a maze using a breadth-first search algorithm. The maze is represented as a 2D grid of characters, where '#' denotes a wall, 'S' denotes the starting position, and 'E' denotes the ending position. The program finds the shortest path from the starting position to the ending position, if one exists, and prints the path along with its length. If there is no path, the program prints "NO".

Here is a step-by-step explanation of the code:

1. The `Solution` class contains all the necessary methods and variables to solve the maze.

2. The `bfs` method performs a breadth-first search starting from the given starting position. It keeps track of the distance from the starting position to each cell and the parent of each cell in the shortest path.

3. The `printPath` method prints the shortest path from the ending position to the starting position, if one exists.

4. The `main` method reads the input, which includes the dimensions of the maze and the maze itself. It then calls the `bfs` method to find the shortest path. Finally, it prints the output, which includes whether a path exists or not, the length of the shortest path, and the shortest path itself.