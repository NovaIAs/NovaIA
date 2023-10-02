Claro! Aqui está um código complexo em Java que implementa um algoritmo de busca em profundidade em um grafo direcionado:

```java
import java.util.*;

class Grafo {
    private int V;
    private LinkedList<Integer> adj[];

    Grafo(int v) {
        V = v;
        adj = new LinkedList[v];
        for (int i = 0; i < v; ++i)
            adj[i] = new LinkedList();
    }

    void addAresta(int v, int w) {
        adj[v].add(w);
    }

    void DFSUtil(int v, boolean visitado[]) {
        visitado[v] = true;
        System.out.print(v + " ");

        Iterator<Integer> i = adj[v].listIterator();
        while (i.hasNext()) {
            int n = i.next();
            if (!visitado[n])
                DFSUtil(n, visitado);
        }
    }

    void DFS(int v) {
        boolean visitado[] = new boolean[V];
        DFSUtil(v, visitado);
    }

    public static void main(String args[]) {
        Grafo g = new Grafo(4);

        g.addAresta(0, 1);
        g.addAresta(0, 2);
        g.addAresta(1, 2);
        g.addAresta(2, 0);
        g.addAresta(2, 3);
        g.addAresta(3, 3);

        System.out.println("Busca em Profundidade (DFS) a partir do vértice 2:");
        g.DFS(2);
    }
}
```

Este código cria uma classe `Grafo` que representa um grafo direcionado utilizando uma lista de adjacências. O algoritmo de busca em profundidade (DFS) é implementado com os métodos `DFSUtil` e `DFS`.

No método `main`, é criado um grafo com 4 vértices e 6 arestas. Em seguida, é chamado o método `DFS` a partir do vértice 2, imprimindo o caminho percorrido.

Espero que este código atenda às suas expectativas de complexidade e originalidade.