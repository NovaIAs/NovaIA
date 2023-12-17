Claro! Aqui está um exemplo de código complexo em Groovy, que implementa um algoritmo de busca em largura (BFS) em um grafo:

```groovy
import java.util.LinkedList
import java.util.Queue

class Grafo {
    int vertices
    LinkedList<Integer>[] adjacencias

    Grafo(int v) {
        vertices = v
        adjacencias = new LinkedList[v]
        for (int i = 0; i < v; ++i) {
            adjacencias[i] = new LinkedList()
        }
    }

    void adicionarAresta(int v, int w) {
        adjacencias[v].add(w)
    }

    void BFS(int s) {
        boolean visitado[] = new boolean[vertices]
        Queue<Integer> fila = new LinkedList<>()
        visitado[s] = true
        fila.add(s)

        while (fila.size() != 0) {
            s = fila.poll()
            print(s + " ")

            Iterator<Integer> i = adjacencias[s].listIterator()
            while (i.hasNext()) {
                int n = i.next()
                if (!visitado[n]) {
                    visitado[n] = true
                    fila.add(n)
                }
            }
        }
    }
}

def g = new Grafo(6)
g.adicionarAresta(0, 1)
g.adicionarAresta(0, 2)
g.adicionarAresta(1, 3)
g.adicionarAresta(2, 4)
g.adicionarAresta(2, 5)

println("A ordem de visitação dos vértices (BFS):")
g.BFS(0)
```

Este código define uma classe `Grafo` que representa um grafo direcionado usando listas de adjacências. A classe possui um construtor para inicializar o número de vértices e um método `adicionarAresta` para adicionar uma aresta entre dois vértices. O método `BFS` implementa o algoritmo de busca em largura.

No exemplo do código, um grafo com 6 vértices é criado e algumas arestas são adicionadas para formar uma estrutura de árvore. O método `BFS` é então chamado com o vértice inicial 0. A ordem de visitação dos vértices é impressa no console.

O algoritmo de busca em largura (BFS) percorre um grafo em largura, visitando todos os vértices alcançáveis a partir do vértice inicial. Ele utiliza uma fila para controlar a ordem de visita dos vértices. No código, a fila é implementada usando a classe `Queue` da biblioteca padrão do Java.