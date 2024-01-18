```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

using namespace std;

// A lambda function to compare two tuples
auto tuple_cmp = [](const tuple<int, int, int>& a, const tuple<int, int, int>& b) {
  return tie(get<0>(a), get<1>(a), get<2>(a)) < tie(get<0>(b), get<1>(b), get<2>(b));
};

// A function to find the minimum number of operations to transform a given array into a sorted array
int min_operations(vector<int>& arr) {
  int n = arr.size();

  // Create a vector to store the minimum number of operations needed to transform a subarray of the given array into a sorted array
  vector<int> dp(n, INT_MAX);

  // Initialize the dp array with the minimum number of operations needed to transform a subarray of length 1 into a sorted array
  for (int i = 0; i < n; i++) {
    dp[i] = 1;
  }

  // Iterate over the array from left to right
  for (int i = 1; i < n; i++) {
    // Iterate over the subarrays of the array ending at the current index
    for (int j = 0; j < i; j++) {
      // If the current element can be appended to the end of the sorted subarray ending at index j, update the minimum number of operations needed to transform the subarray ending at index i into a sorted array
      if (arr[i] >= arr[j]) {
        dp[i] = min(dp[i], dp[j] + 1);
      }
    }
  }

  // Return the minimum number of operations needed to transform the entire array into a sorted array
  return dp[n - 1];
}

// A function to find the longest common subsequence of two strings
string longest_common_subsequence(string str1, string str2) {
  int n = str1.size();
  int m = str2.size();

  // Create a matrix to store the length of the longest common subsequence of the two strings
  vector<vector<int>> dp(n + 1, vector<int>(m + 1, 0));

  // Iterate over the two strings from left to right
  for (int i = 1; i <= n; i++) {
    for (int j = 1; j <= m; j++) {
      // If the current characters of the two strings match, the length of the longest common subsequence is the length of the longest common subsequence of the two strings without the current characters plus 1
      if (str1[i - 1] == str2[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      }
      // Otherwise, the length of the longest common subsequence is the maximum of the lengths of the longest common subsequences of the two strings without the current character of the first string and the current character of the second string
      else {
        dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }

  // Reconstruct the longest common subsequence from the dp matrix
  int i = n;
  int j = m;
  string lcs = "";

  // While the current characters of the two strings match, append the current character of the first string to the longest common subsequence and move to the next character of both strings
  while (i > 0 && j > 0) {
    if (str1[i - 1] == str2[j - 1]) {
      lcs = str1[i - 1] + lcs;
      i--;
      j--;
    }
    // Otherwise, move to the next character of the first string
    else {
      i--;
    }
  }

  // Return the longest common subsequence
  return lcs;
}

// A function to find the diameter of a tree
int diameter_of_tree(vector<vector<int>>& adj, vector<bool>& visited, int root) {
  // Mark the current node as visited
  visited[root] = true;

  // Initialize the diameter of the tree to 0
  int diameter = 0;

  // Iterate over the adjacent nodes of the current node
  for (int i = 0; i < adj[root].size(); i++) {
    // If the adjacent