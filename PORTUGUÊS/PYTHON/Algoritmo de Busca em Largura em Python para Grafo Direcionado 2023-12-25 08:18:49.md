Claro! Vou criar um código complexo em Python para você. Este código irá implementar um algoritmo de busca em largura em um grafo direcionado. Vamos criar uma classe chamada "Grafo" que irá representar o grafo e terá métodos para adicionar vértices e arestas, além de um método para executar a busca em largura.

```python
from collections import defaultdict

class Grafo:
    def __init__(self):
        self.grafo = defaultdict(list)
    
    def adicionar_vertice(self, vertice):
        if vertice not in self.grafo:
            self.grafo[vertice] = []
    
    def adicionar_aresta(self, origem, destino):
        self.grafo[origem].append(destino)
    
    def busca_em_largura(self, vertice_inicial):
        visitados = set()
        fila = [vertice_inicial]
        
        while fila:
            vertice = fila.pop(0)
            if vertice not in visitados:
                print(vertice, end=" ")
                visitados.add(vertice)
                fila.extend(self.grafo[vertice])
```

Aqui está uma explicação do código:

- Na classe `Grafo`, utilizamos o `defaultdict` do módulo `collections` para criar um dicionário onde os valores padrão são listas vazias. Isso nos permite adicionar arestas sem verificar se o vértice já existe no grafo.
- O método `adicionar_vertice` permite adicionar um vértice ao grafo, caso ele não exista.
- O método `adicionar_aresta` permite adicionar uma aresta direcionada entre dois vértices.
- O método `busca_em_largura` implementa o algoritmo de busca em largura. Ele utiliza um conjunto `visitados` para registrar os vértices já visitados e uma lista `fila` para armazenar os vértices que precisam ser visitados. 
- O algoritmo começa a partir do vértice inicial especificado e continua até que não haja mais vértices na fila. Em cada iteração, o vértice é removido da fila, marcado como visitado e seus vértices adjacentes são adicionados à fila.

Esse código é apenas um exemplo de como implementar um algoritmo de busca em largura em Python. Você pode adaptá-lo de acordo com suas necessidades específicas.