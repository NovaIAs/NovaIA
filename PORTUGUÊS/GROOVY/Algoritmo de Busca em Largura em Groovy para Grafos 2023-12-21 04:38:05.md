Claro! Aqui está um exemplo de um código complexo em Groovy que implementa um algoritmo de busca em largura para encontrar a menor distância entre dois vértices em um grafo:

```groovy
import java.util.LinkedList

class Grafo {
    private int numVertices
    private LinkedList<Integer>[] listaAdj

    Grafo(int numVertices) {
        this.numVertices = numVertices
        listaAdj = new LinkedList[numVertices]
        for (int i = 0; i < numVertices; i++) {
            listaAdj[i] = new LinkedList<Integer>()
        }
    }

    void adicionarAresta(int origem, int destino) {
        listaAdj[origem].add(destino)
    }

    int buscarMenorDistancia(int origem, int destino) {
        boolean[] visitado = new boolean[numVertices]
        int[] distancia = new int[numVertices]

        LinkedList<Integer> fila = new LinkedList<Integer>()
        visitado[origem] = true
        distancia[origem] = 0
        fila.add(origem)

        while (fila.size() != 0) {
            int verticeAtual = fila.poll()

            LinkedList<Integer> vizinhos = listaAdj[verticeAtual]
            for (Integer vizinho : vizinhos) {
                if (!visitado[vizinho]) {
                    visitado[vizinho] = true
                    distancia[vizinho] = distancia[verticeAtual] + 1
                    fila.add(vizinho)

                    if (vizinho == destino) {
                        return distancia[vizinho]
                    }
                }
            }
        }

        return -1
    }
}

// Exemplo de uso do algoritmo de busca em largura
def grafo = new Grafo(7)

grafo.adicionarAresta(0, 1)
grafo.adicionarAresta(0, 2)
grafo.adicionarAresta(1, 3)
grafo.adicionarAresta(1, 4)
grafo.adicionarAresta(2, 5)
grafo.adicionarAresta(2, 6)

def origem = 0
def destino = 6
def menorDistancia = grafo.buscarMenorDistancia(origem, destino)

if (menorDistancia != -1) {
    println "A menor distância entre $origem e $destino é $menorDistancia"
} else {
    println "Não existe caminho entre $origem e $destino"
}
```

Neste código, primeiro definimos a classe `Grafo` que possui uma lista de adjacências para representar as arestas do grafo. Em seguida, implementamos o método `adicionarAresta` para adicionar uma aresta ao grafo.

O método `buscarMenorDistancia` realiza a busca em largura a partir do vértice de origem até encontrar o vértice de destino. Utilizamos um array `visitado` para controlar quais vértices já foram visitados, um array `distancia` para armazenar a menor distância de cada vértice até a origem, e uma fila para percorrer os vértices.

Após a definição da classe, criamos uma instância do grafo e adicionamos as arestas. Em seguida, definimos a origem e o destino desejados e chamamos o método `buscarMenorDistancia`. Por fim, exibimos o resultado da busca.

Este código implementa um algoritmo bastante útil em ciência da computação e pode ser aplicado em diversas situações que envolvam grafos, como por exemplo encontrar a menor distância entre duas cidades em um mapa.