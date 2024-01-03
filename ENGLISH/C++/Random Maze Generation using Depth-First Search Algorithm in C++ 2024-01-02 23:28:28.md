Certainly! Here's a complex and unique C++ code that generates a random maze using the Depth-First Search algorithm. This algorithm starts at a random cell and recursively visits unvisited neighbors until it has visited all cells of the maze.

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <stack>

using namespace std;

// Define cell structure
struct Cell {
    bool visited;
    bool upWall;
    bool downWall;
    bool leftWall;
    bool rightWall;

    Cell() : visited(false), upWall(true), downWall(true), leftWall(true), rightWall(true) {}
};

// Generate random integer between min and max (inclusive)
int randomInt(int min, int max) {
    static random_device rd;
    static mt19937 rng(rd());
    uniform_int_distribution<int> uni(min, max);
    return uni(rng);
}

// Generate maze using Depth-First Search algorithm
void generateMaze(vector<vector<Cell>>& maze, int numRows, int numCols) {
    stack<pair<int, int>> stack;
    int startRow = randomInt(0, numRows - 1);
    int startCol = randomInt(0, numCols - 1);

    stack.push(make_pair(startRow, startCol));
    maze[startRow][startCol].visited = true;

    while (!stack.empty()) {
        auto current = stack.top();
        int row = current.first;
        int col = current.second;

        vector<pair<int, int>> neighbors;

        if (row > 0 && !maze[row - 1][col].visited)
            neighbors.push_back(make_pair(row - 1, col));
        if (row < numRows - 1 && !maze[row + 1][col].visited)
            neighbors.push_back(make_pair(row + 1, col));
        if (col > 0 && !maze[row][col - 1].visited)
            neighbors.push_back(make_pair(row, col - 1));
        if (col < numCols - 1 && !maze[row][col + 1].visited)
            neighbors.push_back(make_pair(row, col + 1));

        if (!neighbors.empty()) {
            auto next = neighbors[randomInt(0, neighbors.size() - 1)];
            int nextRow = next.first;
            int nextCol = next.second;

            if (nextRow == row - 1) {
                maze[row][col].upWall = false;
                maze[nextRow][nextCol].downWall = false;
            } else if (nextRow == row + 1) {
                maze[row][col].downWall = false;
                maze[nextRow][nextCol].upWall = false;
            } else if (nextCol == col - 1) {
                maze[row][col].leftWall = false;
                maze[nextRow][nextCol].rightWall = false;
            } else if (nextCol == col + 1) {
                maze[row][col].rightWall = false;
                maze[nextRow][nextCol].leftWall = false;
            }

            maze[nextRow][nextCol].visited = true;
            stack.push(make_pair(nextRow, nextCol));
        } else {
            stack.pop();
        }
    }
}

// Print the maze
void printMaze(const vector<vector<Cell>>& maze) {
    for (const auto& row : maze) {
        for (const auto& cell : row) {
            cout << "+";
            cout << (cell.upWall ? "---" : "   ");
        }
        cout << "+" << endl;
        for (const auto& cell : row) {
            cout << (cell.leftWall ? "|" : " ");
            cout << "   ";
        }
        cout << "|" << endl;
    }
    for (const auto& _ : maze.front()) {
        cout << "+---";
    }
    cout << "+" << endl;
}

int main() {
    int numRows = 10;
    int numCols = 10;

    vector<vector<Cell>> maze(numRows, vector<Cell>(numCols));

    generateMaze(maze, numRows, numCols);
    printMaze(maze);

    return 0;
}
```

This code generates a random maze using the Depth-First Search algorithm. The `generateMaze` function takes a 2D vector representing the maze, the number of rows, and the number of columns as input. It starts at a random cell, marks it as visited, and then iteratively chooses a random unvisited neighbor to visit. The walls between the current cell and the chosen neighbor are removed to create a passage. This process continues until all cells have been visited.

The `printMaze` function prints the maze on the console using ASCII characters. The walls are represented by `---` for horizontal walls and `|` for vertical walls. The passages are represented by empty spaces.

In the `main` function, you can adjust the `numRows` and `numCols` variables to change the size of the maze. The generated maze will be printed on the console.