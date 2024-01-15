```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// A custom comparator function for sorting a vector of pairs by their second element
bool compareBySecond(const pair<int, int> &a, const pair<int, int> &b) {
    return a.second < b.second;
}

// A function to count the number of occurrences of each element in a vector
map<int, int> countOccurrences(const vector<int> &v) {
    map<int, int> counts;
    for (int i = 0; i < v.size(); i++) {
        counts[v[i]]++;
    }
    return counts;
}

// A function to find the longest increasing subsequence in a vector
vector<int> longestIncreasingSubsequence(const vector<int> &v) {
    vector<int> dp(v.size(), 1);
    vector<int> prev(v.size(), -1);
    int longestLength = 1;
    int lastIndex = -1;

    for (int i = 1; i < v.size(); i++) {
        for (int j = 0; j < i; j++) {
            if (v[i] > v[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1;
                prev[i] = j;
                if (dp[i] > longestLength) {
                    longestLength = dp[i];
                    lastIndex = i;
                }
            }
        }
    }

    vector<int> lis;
    while (lastIndex != -1) {
        lis.push_back(v[lastIndex]);
        lastIndex = prev[lastIndex];
    }
    reverse(lis.begin(), lis.end());
    return lis;
}

// A function to find the minimum number of coins needed to make a given amount of money
int minCoins(int amount, vector<int> &coins) {
    vector<int> dp(amount + 1, INT_MAX);
    dp[0] = 0;

    for (int i = 1; i <= amount; i++) {
        for (int j = 0; j < coins.size(); j++) {
            if (i - coins[j] >= 0 && dp[i - coins[j]] != INT_MAX) {
                dp[i] = min(dp[i], dp[i - coins[j]] + 1);
            }
        }
    }

    return dp[amount] == INT_MAX ? -1 : dp[amount];
}

// A function to find the shortest path in a weighted graph using Dijkstra's algorithm
vector<int> dijkstra(const vector<vector<pair<int, int>>> &graph, int source) {
    vector<int> distances(graph.size(), INT_MAX);
    vector<bool> visited(graph.size(), false);

    distances[source] = 0;

    while (true) {
        int minDistance = INT_MAX;
        int minDistanceVertex = -1;

        for (int i = 0; i < graph.size(); i++) {
            if (!visited[i] && distances[i] < minDistance) {
                minDistance = distances[i];
                minDistanceVertex = i;
            }
        }

        if (minDistanceVertex == -1) {
            break;
        }

        visited[minDistanceVertex] = true;

        for (auto &edge : graph[minDistanceVertex]) {
            int destination = edge.first;
            int weight = edge.second;

            if (!visited[destination] && distances[destination] > distances[minDistanceVertex] + weight) {
                distances[destination] = distances[minDistanceVertex] + weight;
            }
        }
    }

    return distances;
}

// A function to find all the subsets of a given set
set<set<int>> subsets(const set<int> &s) {
    set<set<int>> subsets;
    subsets.insert(set<int>());

    for (int i : s) {
        set<set<int>> newSubsets;
        for (set<int> subset : subsets) {
            set<int> newSubset = subset;
            newSubset.insert(i);
            newSubsets.insert(newSubset);
        }
        subsets.insert(newSubsets.begin(), newSubsets.end());
    }

    return subsets;
}

int main() {
    // Example usage of the functions

    // Sorting a vector of pairs by their second element
    vector<pair<int, int>> v = {{1, 3}, {2, 2}, {3, 1}};
    sort(v.begin(), v.end(), compareBySecond);

    // Counting the number of occurrences of each element in a vector
    vector<int> v2 = {1, 2, 3, 4, 5, 1, 2, 3};
    map<int, int> counts = countOccurrences(v2);

    // Finding the longest increasing subsequence in a vector
    vector<int> v3 = {1, 2, 3, 4, 5, 2, 3, 6, 7, 8};
    vector<int> lis = longestIncreasingSubsequence(v3);

    // Finding the minimum number of coins needed to make a given amount of money
    vector<int> coins = {1, 5, 10, 25};
    int amount = 37;
    int min_coins = minCoins(amount, coins);

    // Finding the shortest path in a weighted graph using Dijkstra's algorithm
    vector<vector<pair<int, int>>> graph = {
        {{1, 1}, {2, 3}},
        {{0, 1}, {2, 2}, {3, 4}},
        {{0, 3}, {1, 2}, {3, 1}},
        {{1, 4}, {2, 1}}
    };
    int source = 0;
    vector<int> distances = dijkstra(graph, source);

    // Finding all the subsets of a given set
    set<int> s = {1, 2, 3};
    set<set<int>> subsets_of_s = subsets(s);

    // Printing the results
    for (auto &p : v) {
        cout << p.first << " " << p.second << endl;
    }
    cout << endl;

    for (auto &p : counts) {
        cout << p.first << " " << p.second << endl;
    }
    cout << endl;

    for (int i : lis) {
        cout << i << " ";
    }
    cout << endl;

    cout << "Minimum number of coins: " << min_coins << endl;
    cout << endl;

    for (int i = 0; i < distances.size(); i++) {
        cout << "Distance from source to vertex " << i << ": " << distances[i] << endl;
    }
    cout << endl;

    for (auto &subset : subsets_of_s) {
        for (int i : subset) {
            cout << i << " ";
        }
        cout << endl;
    }

    return 0;
}
```

This code demonstrates various algorithms and data structures in C++, including sorting, counting occurrences, finding the longest increasing subsequence, finding the minimum number of coins needed to make a given amount of money, finding the shortest path in a weighted graph using Dijkstra's algorithm, and finding all the subsets of a given set. The code is well-commented and organized, making it easy to understand and modify.