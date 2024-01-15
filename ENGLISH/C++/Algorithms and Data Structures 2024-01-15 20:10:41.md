```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <string>
#include <sstream>
#include <cmath>
#include <iomanip>
#include <limits>
#include <climits>
#include <cstring>

using namespace std;

#define FOR(i, a, b) for (int i = (a); i < (b); ++i)
#define FORE(i, a, b) for (int i = (a); i <= (b); ++i)
#define FORD(i, a, b) for (int i = (a); i >= (b); --i)

#define REP(i, n) FOR(i, 0, n)
#define REPE(i, n) FORE(i, 0, n)
#define RREP(i, n) FORD(i, n, 0)

#define fi first
#define se second
#define mp make_pair
#define pb push_back
#define ALL(v) (v).begin(), (v).end()
#define ALLA(a, n) (a), (a) + (n)

#define vi vector<int>
#define vvi vector<vi>
#define vl vector<long long>
#define vvl vector<vl>
#define ll long long
#define pii pair<int, int>
#define pll pair<long long, long long>

// Custom Hash Function
struct myhash {
  static size_t const p = 31;
  static size_t const m = 1e9 + 9;
  size_t operator()(const string& s) const {
    size_t h = 0;
    REP(i, s.size()) {
      h = (h * p + s[i] - 'a' + 1) % m;
    }
    return h;
  }
};

// Computes the minimum number of coins needed to make change for a given amount of money.
int coin_change(vector<int>& coins, int amount) {
  vector<int> dp(amount + 1, INT_MAX);
  dp[0] = 0;
  FOR(i, 1, amount + 1) {
    for (auto coin : coins) {
      if (i - coin >= 0 && dp[i - coin] != INT_MAX) {
        dp[i] = min(dp[i], dp[i - coin] + 1);
      }
    }
  }
  return dp[amount] == INT_MAX ? -1 : dp[amount];
}

// Finds the longest common subsequence of two strings.
string lcs(string a, string b) {
  int n = a.size(), m = b.size();
  vector<vector<int>> dp(n + 1, vector<int>(m + 1, 0));
  FOR(i, 1, n + 1) {
    FOR(j, 1, m + 1) {
      if (a[i - 1] == b[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  string lcs;
  int i = n, j = m;
  while (i > 0 && j > 0) {
    if (a[i - 1] == b[j - 1]) {
      lcs += a[i - 1];
      --i;
      --j;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      --i;
    } else {
      --j;
    }
  }
  reverse(lcs.begin(), lcs.end());
  return lcs;
}

// Finds the shortest path between two nodes in a graph.
vector<int> dijkstra(vector<vector<pair<int, int>>>& graph, int start) {
  int n = graph.size();
  vector<int> dist(n, INT_MAX);
  dist[start] = 0;
  priority_queue<pair<int, int>> pq;
  pq.push(mp(0, start));
  while (!pq.empty()) {
    int u = pq.top().second;
    pq.pop();
    for (auto edge : graph[u]) {
      int v = edge.first, weight = edge.second;
      if (dist[v] > dist[u] + weight) {
        dist[v] = dist[u] + weight;
        pq.push(mp(-dist[v], v));
      }
    }
  }
  return dist;
}

// Finds the maximum independent set in a tree.
vector<int> max_independent_set(vector<vector<int>>& graph) {
  int n = graph.size();
  vector<int> dp(n, 0);
  dp[0] = 1;
  FOR(i, 1, n) {
    dp[i] = 1;
    for (auto u : graph[i]) {
      dp[i] = max(dp[i], dp[u] + 1);
    }
  }
  vector<int> mis;
  int max_size = 0;
  FOR(i, 0, n) {
    if (dp[i] > max_size) {
      max_size = dp[i];
      mis = {i};