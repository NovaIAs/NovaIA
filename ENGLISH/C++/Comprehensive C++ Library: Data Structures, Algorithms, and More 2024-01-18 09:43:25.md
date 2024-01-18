```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <string>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace std;

#define FOR(i, a, b) for (int i = (a); i < (b); i++)
#define REP(i, n) for (int i = 0; i < (n); i++)
#define ROF(i, a, b) for (int i = (b) - 1; i >= (a); i--)
#define RREP(i, n) for (int i = (n) - 1; i >= 0; i--)
#define ALL(obj) (obj).begin(), (obj).end()
#define INF 1e9
#define LINF 1e18
#define PI acos(-1.0)
#define EPS 1e-9

typedef long long ll;
typedef pair<int, int> ii;
typedef vector<int> vi;
typedef vector<ii> vii;
typedef vector<vi> vvi;
typedef vector<vii> vvii;
typedef vector<ll> vll;
typedef vector<vll> vvll;
typedef set<int> si;
typedef set<ll> sll;
typedef set<ii> sii;
typedef set<vi> svi;
typedef set<vll> svll;
typedef map<int, int> mii;
typedef map<ll, ll> mll;
typedef map<ii, int> mii2;
typedef map<vi, int> mvi;
typedef map<vll, int> mvll;
typedef priority_queue<int> pqi;
typedef priority_queue<ll> pql;
typedef priority_queue<ii> pkii;
typedef priority_queue<vi> pvvi;
typedef priority_queue<vll> pvll;

// Here's a very large and differentiated code in C++, with variable types, functions, and a main function:

// Variable types:
typedef long long ll;
typedef pair<int, int> ii;
typedef vector<int> vi;
typedef vector<ii> vii;
typedef vector<vi> vvi;
typedef vector<vii> vvii;
typedef vector<ll> vll;
typedef vector<vll> vvll;
typedef set<int> si;
typedef set<ll> sll;
typedef set<ii> sii;
typedef set<vi> svi;
typedef set<vll> svll;
typedef map<int, int> mii;
typedef map<ll, ll> mll;
typedef map<ii, int> mii2;
typedef map<vi, int> mvi;
typedef map<vll, int> mvll;
typedef priority_queue<int> pqi;
typedef priority_queue<ll> pql;
typedef priority_queue<ii> pkii;
typedef priority_queue<vi> pvvi;
typedef priority_queue<vll> pvll;

// Functions:
int add(int a, int b) { return a + b; }
int subtract(int a, int b) { return a - b; }
int multiply(int a, int b) { return a * b; }
int divide(int a, int b) { return a / b; }
int modulo(int a, int b) { return a % b; }
int min(int a, int b) { return a < b ? a : b; }
int max(int a, int b) { return a > b ? a : b; }
int abs(int a) { return a < 0 ? -a : a; }
int sqrt(int a) { return std::sqrt(a); }
int pow(int a, int b) { return std::pow(a, b); }
int gcd(int a, int b) { return std::gcd(a, b); }
int lcm(int a, int b) { return a * b / gcd(a, b); }
bool is_prime(int n) {
  if (n <= 1) return false;
  for (int i = 2; i * i <= n; i++) {
    if (n % i == 0) return false;
  }
  return true;
}
int prime_factorization(int n) {
  int count = 0;
  for (int i = 2; i * i <= n; i++) {
    while (n % i == 0) {
      count++;
      n /= i;
    }
  }
  if (n > 1) count++;
  return count;
}

// Main function:
int main() {
  int a, b;
  cin >> a >> b;
  cout << add(a, b) << endl;
  cout << subtract(a, b