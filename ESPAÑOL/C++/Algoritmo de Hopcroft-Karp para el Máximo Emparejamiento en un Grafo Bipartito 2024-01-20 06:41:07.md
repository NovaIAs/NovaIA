```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace std;

typedef long long ll;
typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<pii> vpii;
typedef vector<ll> vll;
typedef vector<vll> vvll;
typedef vector<string> vs;
typedef vector<vs> vvs;
typedef map<int, int> mii;
typedef set<int> si;
typedef queue<int> qi;
typedef stack<int> si;

#define pb push_back
#define mp make_pair
#define fi first
#define se second
#define FOR(i, a, b) for (int i = a; i <= b; i++)
#define FOD(i, a, b) for (int i = a; i >= b; i--)
#define REP(i, n) for (int i = 0; i < n; i++)
#define endl '\n'

// ######################################################################### //

const int MAXN = 1e6 + 5;

int n, m, k;
int a[MAXN], b[MAXN], c[MAXN];
vi adj[MAXN];
int vis[MAXN];
int match[MAXN];
int dist[MAXN];
int parent[MAXN];

// ######################################################################### //

bool bfs() {
  queue<int> q;
  REP(i, n) {
    if (!vis[i]) {
      q.push(i);
      dist[i] = 0;
    } else
      dist[i] = MAXN;
  }
  while (!q.empty()) {
    int u = q.front();
    q.pop();
    for (int v : adj[u]) {
      if (match[v] == -1 || dist[match[v]] == dist[u] + 1) {
        dist[v] = dist[u] + 1;
        if (match[v] == -1) {
          parent[v] = u;
          return true;
        } else {
          parent[match[v]] = u;
          q.push(match[v]);
        }
      }
    }
  }
  return false;
}

int dfs(int u) {
  if (vis[u]) return 0;
  vis[u] = true;
  for (int v : adj[u]) {
    if (match[v] == -1 || dfs(match[v])) {
      match[v] = u;
      match[u] = v;
      return 1;
    }
  }
  return 0;
}

int maxMatching() {
  memset(match, -1, sizeof match);
  int ans = 0;
  while (bfs()) {
    REP(i, n) {
      if (!vis[i] && dfs(i)) ans++;
    }
    memset(vis, 0, sizeof vis);
  }
  return ans;
}

// ######################################################################### //

int main() {
  cin >> n >> m >> k;
  REP(i, n) cin >> a[i];
  REP(i, m) cin >> b[i];
  REP(i, k) {
    int u, v;
    cin >> u >> v;
    u--, v--;
    adj[u].pb(v);
  }
  cout << maxMatching() << endl;
  return 0;
}
```

El código anterior implementa el algoritmo de Hopcroft–Karp para encontrar el máximo emparejamiento en un grafo bipartito. El grafo bipartito es un grafo en el que los vértices se pueden dividir en dos conjuntos disjuntos, llamados conjuntos de vértices izquierdo y derecho, y todos los bordes conectan un vértice del conjunto de vértices izquierdo con un vértice del conjunto de vértices derecho.

El algoritmo de Hopcroft–Karp utiliza una técnica de búsqueda en amplitud (BFS) para encontrar una ruta de aumento en el grafo bipartito. Una ruta de aumento es un camino de longitud impar desde un vértice no emparejado en el conjunto de vértices izquierdo hasta un vértice no emparejado en el conjunto de vértices derecho. Una vez que se encuentra una ruta de aumento, se puede utilizar para aumentar el tamaño del emparejamiento máximo mediante la adición de un nuevo borde al emparejamiento.

El algoritmo de Hopcroft–Karp se repite hasta que no se encuentren más rutas de aumento, en cuyo momento el emparejamiento máximo se ha encontrado.

La complejidad del algoritmo de Hopcroft–Karp es O(sqrt(V) * E), donde V es el número de vértices en el grafo bipartito y E es el número de bordes en el grafo bipartito.