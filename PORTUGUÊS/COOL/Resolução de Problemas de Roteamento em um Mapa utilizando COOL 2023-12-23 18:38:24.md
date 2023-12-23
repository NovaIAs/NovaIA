Claro! Aqui está um exemplo de um código complexo em COOL que utiliza estruturas de dados avançadas e algoritmos sofisticados para resolver um problema de roteamento em um mapa:

```
class Roteador {
    atributo mapa: Mapa;
    atributo distancia: Array<Int>;
    atributo visitado: Array<Bool>;
    
    inicializar() : Object {
        mapa <- new Mapa;
        distancia <- new Array<Int>(mapa.numVertices());
        visitado <- new Array<Bool>(mapa.numVertices());
        para cada v em mapa.vertices() faça {
            distancia[v] <- infinito;
            visitado[v] <- falso;
        }
        retornar this;
    }
    
    função dijkstra(origem: Int) : Object {
        distancia[origem] <- 0;
        para cada _ em mapa.vertices() faça {
            var v <- encontrarVerticeMenorDistanciaNaoVisitado();
            visitado[v] <- verdadeiro;
            para cada u em mapa.vizinhos(v) faça {
                se (distancia[u] > distancia[v] + mapa.peso(v, u)) então {
                    distancia[u] <- distancia[v] + mapa.peso(v, u);
                }
            }
        }
        retornar this;
    }
    
    função encontrarVerticeMenorDistanciaNaoVisitado() : Int {
        var menorDistancia <- infinito;
        var verticeMenorDistancia <- -1;
        para cada v em mapa.vertices() faça {
            se (!visitado[v] e distancia[v] < menorDistancia) então {
                menorDistancia <- distancia[v];
                verticeMenorDistancia <- v;
            }
        }
        retornar verticeMenorDistancia;
    }
}

class Mapa {
    atributo vertices: Array<Vertex>;
    
    inicializar() : Object {
        vertices <- new Array<Vertex>;
        retornar this;
    }
    
    função numVertices() : Int {
        retornar vertices.tamanho();
    }
    
    função vertices() : Array<Vertex> {
        retornar vertices;
    }
    
    função vizinhos(v: Int) : Array<Int> {
        retornar vertices[v].vizinhos();
    }
    
    função peso(u: Int, v: Int) : Int {
        retornar vertices[u].peso(v);
    }
}

class Vertex {
    atributo vizinhos: Array<Int>;
    atributo pesos: Array<Int>;
    
    inicializar() : Object {
        vizinhos <- new Array<Int>;
        pesos <- new Array<Int>;
        retornar this;
    }
    
    função vizinhos() : Array<Int> {
        retornar vizinhos;
    }
    
    função peso(v: Int) : Int {
        var indice <- vizinhos.encontrarIndice(v);
        retornar pesos[indice];
    }
}
```

Neste exemplo, temos três classes: Roteador, Mapa e Vertex. A classe Roteador é responsável por realizar o algoritmo de Dijkstra para encontrar o caminho mais curto entre dois vértices em um mapa. A classe Mapa representa o mapa do problema, contendo uma lista de vértices. A classe Vertex representa cada vértice do mapa, com seus vizinhos e pesos das arestas.

O código utiliza estruturas de dados como arrays e loops para percorrer os vértices, calcular as distâncias mínimas e encontrar o próximo vértice a ser visitado. O algoritmo de Dijkstra é implementado na função `dijkstra` da classe Roteador, enquanto a função `encontrarVerticeMenorDistanciaNaoVisitado` é usada para encontrar o vértice com a menor distância não visitado.

Essa implementação complexa pode ser adaptada para diversos cenários de roteamento em mapas, como encontrar a rota mais curta entre cidades em um sistema de navegação ou otimizar o fluxo de dados em uma rede de computadores.