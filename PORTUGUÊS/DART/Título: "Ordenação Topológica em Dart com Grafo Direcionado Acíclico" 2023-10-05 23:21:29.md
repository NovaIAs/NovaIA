Claro! Aqui está um código complexo em Dart, que envolve a criação de uma estrutura de dados avançada chamada Grafo Direcionado Acíclico (DAG). Vou explicar cada parte do código para facilitar o entendimento.

```dart
class Vertice {
  String nome;
  List<Vertice> adjacentes;

  Vertice(this.nome) {
    adjacentes = [];
  }
}

class DAG {
  List<Vertice> vertices;

  DAG() {
    vertices = [];
  }

  void adicionarVertice(String nome) {
    vertices.add(Vertice(nome));
  }

  void adicionarAresta(String origem, String destino) {
    Vertice vOrigem = _encontrarVertice(origem);
    Vertice vDestino = _encontrarVertice(destino);

    if (vOrigem != null && vDestino != null) {
      vOrigem.adjacentes.add(vDestino);
    }
  }

  List<Vertice> ordenacaoTopologica() {
    List<Vertice> ordenacao = [];
    Set<Vertice> visitados = {};

    for (var vertice in vertices) {
      _visitar(vertice, ordenacao, visitados);
    }

    return ordenacao.reversed.toList();
  }

  void _visitar(Vertice vertice, List<Vertice> ordenacao, Set<Vertice> visitados) {
    if (!visitados.contains(vertice)) {
      visitados.add(vertice);

      for (var adjacente in vertice.adjacentes) {
        _visitar(adjacente, ordenacao, visitados);
      }

      ordenacao.add(vertice);
    }
  }

  Vertice _encontrarVertice(String nome) {
    for (var vertice in vertices) {
      if (vertice.nome == nome) {
        return vertice;
      }
    }

    return null;
  }
}

void main() {
  DAG dag = DAG();

  dag.adicionarVertice("A");
  dag.adicionarVertice("B");
  dag.adicionarVertice("C");
  dag.adicionarVertice("D");
  dag.adicionarVertice("E");

  dag.adicionarAresta("A", "B");
  dag.adicionarAresta("B", "C");
  dag.adicionarAresta("B", "D");
  dag.adicionarAresta("C", "E");
  dag.adicionarAresta("D", "E");

  List<Vertice> ordenacao = dag.ordenacaoTopologica();

  print("Ordem topológica:");
  for (var vertice in ordenacao) {
    print(vertice.nome);
  }
}
```

Este código cria uma estrutura de Grafo Direcionado Acíclico (DAG) e realiza uma ordenação topológica dos seus vértices. 

Primeiro, temos a classe `Vertice`, que representa um nó no grafo. Cada vértice possui um nome e uma lista de vértices adjacentes. 

A classe `DAG` é responsável por armazenar os vértices do grafo e realizar a ordenação topológica. Ela possui um método `adicionarVertice` para adicionar vértices ao grafo e um método `adicionarAresta` para adicionar arestas entre os vértices.

O método `ordenacaoTopologica` retorna uma lista com a ordem topológica dos vértices. Ele utiliza um conjunto `visitados` para controlar quais vértices já foram visitados durante a ordenação. O método `_visitar` é utilizado para percorrer recursivamente os vértices adjacentes a partir de um vértice atual, adicionando-os na ordem inversa à lista `ordenacao`.

O método `_encontrarVertice` é um método auxiliar para buscar um vértice pelo seu nome no grafo.

Por fim, a função `main` cria uma instância da classe `DAG`, adiciona vértices e arestas ao grafo e realiza a ordenação topológica. O resultado é impresso no console.

Esse código é apenas um exemplo de como criar e utilizar uma estrutura de Grafo Direcionado Acíclico em Dart. A complexidade e tamanho podem variar dependendo da quantidade de vértices e arestas no grafo.