Claro! Aqui está um código complexo em Groovy que demonstra a implementação de um algoritmo de busca em largura em um grafo:

```groovy
class Grafo {
    Map<String, List<String>> adjacencias

    Grafo() {
        adjacencias = [:]
    }

    void adicionarVertice(String vertice) {
        adjacencias[vertice] = []
    }

    void adicionarAresta(String vertice1, String vertice2) {
        adjacencias[vertice1] << vertice2
        adjacencias[vertice2] << vertice1
    }

    List<String> buscaEmLargura(String verticeInicial) {
        def visitados = [verticeInicial]
        def fila = [verticeInicial]
        def resultado = []

        while (fila) {
            def verticeAtual = fila.remove(0)
            resultado << verticeAtual

            adjacencias[verticeAtual].each {
                if (!visitados.contains(it)) {
                    visitados << it
                    fila << it
                }
            }
        }

        return resultado
    }
}

def grafo = new Grafo()

grafo.adicionarVertice("A")
grafo.adicionarVertice("B")
grafo.adicionarVertice("C")
grafo.adicionarVertice("D")
grafo.adicionarVertice("E")
grafo.adicionarVertice("F")
grafo.adicionarVertice("G")
grafo.adicionarVertice("H")

grafo.adicionarAresta("A", "B")
grafo.adicionarAresta("A", "C")
grafo.adicionarAresta("B", "D")
grafo.adicionarAresta("B", "E")
grafo.adicionarAresta("C", "F")
grafo.adicionarAresta("C", "G")
grafo.adicionarAresta("D", "H")

def resultadoBusca = grafo.buscaEmLargura("A")
println "Resultado da busca em largura a partir do vértice A: $resultadoBusca"
```

Neste código, criamos uma classe `Grafo` que possui um mapa chamado `adjacencias` para armazenar as adjacências de cada vértice. O método `adicionarVertice` é usado para adicionar um novo vértice ao grafo, enquanto o método `adicionarAresta` é usado para adicionar uma aresta entre dois vértices.

A implementação do algoritmo de busca em largura está no método `buscaEmLargura`. O algoritmo utiliza uma fila para percorrer os vértices do grafo em largura. Começando pelo vértice inicial, adicionamos seus vizinhos não visitados à fila e repetimos o processo até que todos os vértices sejam visitados.

No exemplo fornecido, criamos um grafo com oito vértices e sete arestas. Em seguida, realizamos a busca em largura a partir do vértice "A" e exibimos o resultado.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.