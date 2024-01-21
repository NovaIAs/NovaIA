```c++
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

class Grafo {
public:
  Grafo(int num_nodos) : _num_nodos(num_nodos) {
    _adyacencia.resize(_num_nodos);
  }

  void anadir_arista(int nodo_origen, int nodo_destino, int peso) {
    _adyacencia[nodo_origen].push_back({nodo_destino, peso});
  }

  vector<pair<int, int>> _adyacencia;
  int _num_nodos;
};

struct Arista {
  int nodo_origen;
  int nodo_destino;
  int peso;
};

class Kruskal {
public:
  Kruskal(Grafo& grafo) {
    _grafo = grafo;
    _num_nodos = _grafo._num_nodos;
    _parent.resize(_num_nodos);
    for (int i = 0; i < _num_nodos; i++) {
      _parent[i] = i;
    }

    _aristas = extraer_aristas(_grafo);
    sort(_aristas.begin(), _aristas.end(), [](const Arista& a, const Arista& b) {
      return a.peso < b.peso;
    });
  }

  vector<Arista> _aristas;
  Grafo& _grafo;
  int _num_nodos;
  vector<int> _parent;

  vector<Arista> extraer_aristas(Grafo& grafo) {
    vector<Arista> aristas;
    for (int i = 0; i < _num_nodos; i++) {
      for (const auto& arista : grafo._adyacencia[i]) {
        aristas.push_back({i, arista.first, arista.second});
      }
    }
    return aristas;
  }

  int find_parent(int nodo) {
    if (_parent[nodo] == nodo) {
      return nodo;
    }
    return _parent[nodo] = find_parent(_parent[nodo]);
  }

  void union(int nodo_a, int nodo_b) {
    int padre_a = find_parent(nodo_a);
    int padre_b = find_parent(nodo_b);
    if (padre_a != padre_b) {
      _parent[padre_b] = padre_a;
    }
  }

  vector<Arista> ejecutar() {
    vector<Arista> arbol_minimo_de_coste;
    for (const auto& arista : _aristas) {
      int padre_a = find_parent(arista.nodo_origen);
      int padre_b = find_parent(arista.nodo_destino);
      if (padre_a != padre_b) {
        arbol_minimo_de_coste.push_back(arista);
        union(padre_a, padre_b);
      }
    }
    return arbol_minimo_de_coste;
  }
};

int main() {
  Grafo grafo(9);
  grafo.anadir_arista(0, 1, 4);
  grafo.anadir_arista(0, 6, 7);
  grafo.anadir_arista(1, 6, 11);
  grafo.anadir_arista(1, 7, 20);
  grafo.anadir_arista(1, 2, 9);
  grafo.anadir_arista(2, 3, 6);
  grafo.anadir_arista(2, 5, 2);
  grafo.anadir_arista(3, 4, 10);
  grafo.anadir_arista(3, 5, 5);
  grafo.anadir_arista(4, 5, 15);
  grafo.anadir_arista(4, 7, 1);
  grafo.anadir_arista(4, 8, 5);
  grafo.anadir_arista(5, 6, 9);
  grafo.anadir_arista(6, 7, 1);
  grafo.anadir_arista(7, 8, 3);

  Kruskal kruskal(grafo);
  vector<Arista> arbol_minimo_de_coste = kruskal.ejecutar();

  for (const auto& arista : arbol_minimo_de_coste) {
    cout << "Nodo origen: " << arista.nodo_origen << endl;
    cout << "Nodo destino: " << arista.nodo_destino << endl;
    cout << "Peso: " << arista.peso << endl;
    cout << "------------------" << endl;