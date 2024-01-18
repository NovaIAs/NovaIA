```java
import java.util.*;
import java.io.*;
import java.math.*;

public class Main {
    static int n, m, q;
    static int[][] grid;
    static int[][][] dp;
    static int[][][] sum;

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        n = Integer.parseInt(st.nextToken());
        m = Integer.parseInt(st.nextToken());
        q = Integer.parseInt(st.nextToken());
        grid = new int[n + 1][m + 1];
        dp = new int[n + 1][m + 1][3];
        sum = new int[n + 1][m + 1][3];
        for (int i = 1; i <= n; i++) {
            st = new StringTokenizer(br.readLine());
            for (int j = 1; j <= m; j++) {
                grid[i][j] = Integer.parseInt(st.nextToken());
            }
        }
        preprocess();
        while (q-- > 0) {
            st = new StringTokenizer(br.readLine());
            int x1 = Integer.parseInt(st.nextToken());
            int y1 = Integer.parseInt(st.nextToken());
            int x2 = Integer.parseInt(st.nextToken());
            int y2 = Integer.parseInt(st.nextToken());
            int k = Integer.parseInt(st.nextToken());
            System.out.println(query(x1, y1, x2, y2, k));
        }
    }

    static void preprocess() {
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= m; j++) {
                sum[i][j][0] = grid[i][j];
                sum[i][j][1] = sum[i - 1][j][0] + sum[i][j - 1][0] - sum[i - 1][j - 1][0];
                sum[i][j][2] = sum[i - 1][j][1] + sum[i][j - 1][1] - sum[i - 1][j - 1][1];
            }
        }
    }

    static int query(int x1, int y1, int x2, int y2, int k) {
        int ans = 0;
        if (k == 0) {
            ans = sum[x2][y2][0] - sum[x1 - 1][y2][0] - sum[x2][y1 - 1][0] + sum[x1 - 1][y1 - 1][0];
        } else if (k == 1) {
            ans = sum[x2][y2][1] - sum[x1 - 1][y2][1] - sum[x2][y1 - 1][1] + sum[x1 - 1][y1 - 1][1];
        } else {
            ans = sum[x2][y2][2] - sum[x1 - 1][y2][2] - sum[x2][y1 - 1][2] + sum[x1 - 1][y1 - 1][2];
        }
        return ans;
    }
}
```

This Java code implements a solution to a complex problem involving grid queries and range sum calculations. Here's a breakdown of the code:

1. **Input and Initialization**:
   - Read the dimensions of the grid `n`, `m`, and the number of queries `q`.
   - Create a 2D grid `grid` to store the grid values and 3D arrays `dp` and `sum` for dynamic programming and prefix sum calculations.

2. **Preprocessing**:
   - Calculate the prefix sums for each cell in the grid for three different cases:
     - `sum[i][j][0]`: Regular prefix sum.
     - `sum[i][j][1]`: Prefix sum of the cells in the current row.
     - `sum[i][j][2]`: Prefix sum of the cells in the current column.

3. **Query Handling**:
   - For each query, read the coordinates `(x1, y1)` and `(x2, y2)` of the query rectangle and an integer `k`.
   - Calculate the sum of elements in the query rectangle based on the value of `k`:
     - If `k == 0`, calculate the sum of elements in the rectangle.
     - If `k == 1`, calculate the sum of elements in the current row within the rectangle.
     - If `k == 2`, calculate the sum of elements in the current column within the rectangle.

4. **Output**:
   - Print the result of the query.