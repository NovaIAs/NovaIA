Claro! Aqui está um código em COOL que implementa um algoritmo de busca por profundidade em um grafo:

```
class Grafo {
    propriedade vertices : Int <- 0;
    propriedade arestas : Array de Array de Bool <- new Array of Array of Bool.

    metodo adicionarVertice() : SELF_TYPE {
        vertices <- vertices + 1;
        arestas <- arestas.withappended(new Array of Bool);
        arestas.foreach(a : Array of Bool => a <- a.withappended(false));
        arestas[vertices - 1] <- new Array of Bool with_all(true);
        retorno eu;
    }

    metodo adicionarAresta(v1 : Int, v2 : Int) : SELF_TYPE {
        arestas[v1 - 1][v2 - 1] <- true;
        arestas[v2 - 1][v1 - 1] <- true;
        retorno eu;
    }

    metodo buscaProfundidade(v : Int, visitados : Array de Bool, ordem : Array de Int) : Int {
        visitados[v - 1] <- true;
        ordem.withappended(v);
        for vizinho in 1..vertices loop
            if arestas[v - 1][vizinho - 1] and not visitados[vizinho - 1] then
                buscaProfundidade(vizinho, visitados, ordem);
            fim;
        fim;
        retorno eu;
    }

    metodo buscaProfundidadeGrafo() : Array de Int {
        var visitados : Array de Bool <- new Array of Bool with_all(false);
        var ordem : Array de Int <- new Array of Int;
        for v in 1..vertices loop
            if not visitados[v - 1] then
                buscaProfundidade(v, visitados, ordem);
            fim;
        fim;
        retorno ordem;
    }
}

metodo main() : Object {
    var g : Grafo <- new Grafo;
    g.adicionarVertice();
    g.adicionarVertice();
    g.adicionarVertice();
    g.adicionarVertice();
    g.adicionarAresta(1, 2);
    g.adicionarAresta(1, 3);
    g.adicionarAresta(2, 4);
    g.adicionarAresta(3, 4);

    var ordem : Array de Int <- g.buscaProfundidadeGrafo();
    out_string("Ordem da busca em profundidade: ");
    ordem.foreach(o : Int => out_int(o); out_string(" "));
    out_string("\n");

    retorno 0;
}
```

Neste código, criamos uma classe chamada `Grafo` que representa um grafo não direcionado. O grafo é implementado utilizando uma matriz de adjacência, onde cada elemento `arestas[i][j]` indica se existe uma aresta entre o vértice `i` e o vértice `j`.

A classe `Grafo` possui métodos para adicionar vértices e arestas, bem como o método `buscaProfundidadeGrafo`, que realiza a busca por profundidade no grafo. A busca por profundidade visita todos os vértices do grafo de forma recursiva, marcando-os como visitados e adicionando-os em uma ordem específica.

O método `main` é o ponto de entrada do programa, onde criamos um grafo com quatro vértices e adicionamos algumas arestas. Em seguida, chamamos o método `buscaProfundidadeGrafo` para obter a ordem da busca em profundidade e exibimos o resultado na saída.

Espero que este código atenda às suas expectativas!