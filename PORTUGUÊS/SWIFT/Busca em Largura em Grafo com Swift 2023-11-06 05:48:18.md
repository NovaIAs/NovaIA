Com certeza! Aqui está um código complexo em Swift que realiza uma busca em largura em um grafo representado por uma matriz de adjacência.

```swift
class Grafo {
    var numVertices: Int
    var matrizAdjacencia: [[Int]]
    
    init(numVertices: Int) {
        self.numVertices = numVertices
        self.matrizAdjacencia = Array(repeating: Array(repeating: 0, count: numVertices), count: numVertices)
    }
    
    func adicionarAresta(origem: Int, destino: Int) {
        matrizAdjacencia[origem][destino] = 1
        matrizAdjacencia[destino][origem] = 1
    }
    
    func buscaLargura(inicio: Int) {
        var visitado = Array(repeating: false, count: numVertices)
        var fila = [Int]()
        
        visitado[inicio] = true
        fila.append(inicio)
        
        while !fila.isEmpty {
            let verticeAtual = fila.removeFirst()
            print("Visitando vértice: \(verticeAtual)")
            
            for i in 0..<numVertices {
                if matrizAdjacencia[verticeAtual][i] == 1 && !visitado[i] {
                    visitado[i] = true
                    fila.append(i)
                }
            }
        }
    }
}

// Exemplo de uso
let grafo = Grafo(numVertices: 6)
grafo.adicionarAresta(origem: 0, destino: 1)
grafo.adicionarAresta(origem: 0, destino: 2)
grafo.adicionarAresta(origem: 1, destino: 3)
grafo.adicionarAresta(origem: 1, destino: 4)
grafo.adicionarAresta(origem: 2, destino: 4)
grafo.adicionarAresta(origem: 3, destino: 4)
grafo.adicionarAresta(origem: 3, destino: 5)
grafo.adicionarAresta(origem: 4, destino: 5)

grafo.buscaLargura(inicio: 0)
```

Neste código, a classe `Grafo` representa um grafo não direcionado utilizando uma matriz de adjacência. A função `adicionarAresta` é utilizada para adicionar uma aresta entre dois vértices. A função `buscaLargura` realiza a busca em largura a partir de um vértice inicial. 

No exemplo de uso, são adicionadas arestas ao grafo e em seguida é realizada uma busca em largura a partir do vértice 0. Durante a busca, cada vértice visitado é exibido no console.