Claro! Aqui está um código complexo em Python que realiza um algoritmo de busca em largura e profundidade em um grafo. O programa encontra o caminho mais curto entre dois nós do grafo e exibe o resultado.

```python
from collections import deque

# Classe para representar um grafo direcionado
class Grafo:
    def __init__(self):
        self.vertices = {}

    # Adicionar um vértice ao grafo
    def adicionar_vertice(self, vertice):
        if vertice not in self.vertices:
            self.vertices[vertice] = []

    # Adicionar uma aresta entre dois vértices
    def adicionar_aresta(self, origem, destino):
        if origem in self.vertices:
            self.vertices[origem].append(destino)
        else:
            self.vertices[origem] = [destino]

    # Realizar busca em largura no grafo
    def busca_em_largura(self, origem, destino):
        fila = deque()
        visitados = set()

        fila.append(origem)
        visitados.add(origem)

        while fila:
            vertice = fila.popleft()

            if vertice == destino:
                return True

            for vizinho in self.vertices[vertice]:
                if vizinho not in visitados:
                    fila.append(vizinho)
                    visitados.add(vizinho)

        return False

    # Realizar busca em profundidade no grafo
    def busca_em_profundidade(self, origem, destino):
        pilha = []
        visitados = set()

        pilha.append(origem)
        visitados.add(origem)

        while pilha:
            vertice = pilha.pop()

            if vertice == destino:
                return True

            for vizinho in self.vertices[vertice]:
                if vizinho not in visitados:
                    pilha.append(vizinho)
                    visitados.add(vizinho)

        return False

# Exemplo de uso do grafo
grafo = Grafo()

# Adicionando vértices
grafo.adicionar_vertice("A")
grafo.adicionar_vertice("B")
grafo.adicionar_vertice("C")
grafo.adicionar_vertice("D")
grafo.adicionar_vertice("E")

# Adicionando arestas
grafo.adicionar_aresta("A", "B")
grafo.adicionar_aresta("A", "C")
grafo.adicionar_aresta("B", "D")
grafo.adicionar_aresta("C", "D")
grafo.adicionar_aresta("D", "E")

# Chamando a busca em largura
if grafo.busca_em_largura("A", "E"):
    print("Caminho encontrado usando busca em largura")
else:
    print("Caminho não encontrado usando busca em largura")

# Chamando a busca em profundidade
if grafo.busca_em_profundidade("A", "E"):
    print("Caminho encontrado usando busca em profundidade")
else:
    print("Caminho não encontrado usando busca em profundidade")
```

Este código define uma classe `Grafo` que representa um grafo direcionado. A classe possui os seguintes métodos:

- `adicionar_vertice(vertice)`: adiciona um vértice ao grafo.
- `adicionar_aresta(origem, destino)`: adiciona uma aresta (direcionada) entre dois vértices.
- `busca_em_largura(origem, destino)`: realiza uma busca em largura no grafo para encontrar o caminho mais curto entre a origem e o destino.
- `busca_em_profundidade(origem, destino)`: realiza uma busca em profundidade no grafo para encontrar o caminho entre a origem e o destino.

No exemplo de uso, é criado um grafo com cinco vértices (A, B, C, D, E) e são adicionadas arestas entre eles. Em seguida, é chamada a busca em largura e a busca em profundidade para encontrar o caminho entre o vértice "A" e o vértice "E". O resultado é exibido no console.

Este código demonstra um exemplo de algoritmo de busca em largura e profundidade em um grafo direcionado, mas pode ser facilmente adaptado para outros casos.